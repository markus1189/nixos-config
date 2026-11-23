;;; mh-annotate-test.el --- Tests for mh-annotate.el  -*- lexical-binding: t; -*-

;; Run through the flake: nix build --no-link .#checks.x86_64-linux.mh-annotate-ert
;; That uses the emacs the hosts install, so an emacs-overlay bump that changes
;; annotate's internals fails here and not in a review.

(require 'ert)
(require 'mh-annotate)

(defvar mh/annotate-test--dir nil)

(defmacro mh/annotate-test--sandbox (&rest body)
  "Run BODY with a database of its own and annotate set up as in emacs-config.el.
Any prompt is an error: take runs through emacsclient, where nobody answers."
  (declare (indent 0))
  `(let* ((mh/annotate-test--dir (file-truename (make-temp-file "mh-annotate-test" t)))
          (annotate-file (expand-file-name "db" mh/annotate-test--dir))
          (annotate-use-messages nil)
          (annotate-warn-if-hash-mismatch nil)
          (annotate-annotation-confirm-deletion t)
          (annotate-database-confirm-deletion t)
          (mh/annotate--db-cache nil)
          (mh/annotate--items nil)
          (mh/annotate--marks (make-hash-table :test #'equal))
          (kill-ring nil))
     (unwind-protect
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Prompted")))
                   ((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "Prompted")))
                   ((symbol-function 'lwarn) #'ignore))
           ,@body)
       (dolist (buffer (buffer-list))
         (when (and (buffer-file-name buffer)
                    (string-prefix-p mh/annotate-test--dir (file-truename (buffer-file-name buffer))))
           (with-current-buffer buffer
             (when annotate-mode (annotate-mode -1))
             (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (when (get-buffer mh/annotate--review-buffer) (kill-buffer mh/annotate--review-buffer))
       (delete-directory mh/annotate-test--dir t))))

(defun mh/annotate-test--file (name content)
  (let ((file (expand-file-name name mh/annotate-test--dir)))
    (make-directory (file-name-directory file) t)
    (write-region content nil file nil 'silent)
    file))

(defun mh/annotate-test--uuid (age)
  "A UUIDv1 as annotate writes them, AGE seconds old."
  (let ((ticks (+ (floor (* (- (float-time) age) 1e7)) #x01b21dd213814000)))
    (format "%08x-%04x-%04x-%04x-%012x"
            (logand #xffffffff ticks) (logand #xffff (ash ticks -32))
            (logior #x1000 (logand #xfff (ash ticks -48)))
            (logior #x8000 (random #x4000)) (random #xffffffffffff))))

(defun mh/annotate-test--note (file needle note &optional age)
  "Annotate NEEDLE in FILE with NOTE, AGE seconds ago; the buffer stays open."
  (with-current-buffer (find-file-noselect file)
    (unless annotate-mode (annotate-mode 1))
    (goto-char (point-min))
    (search-forward needle)
    (annotate-create-annotation (match-beginning 0) (match-end 0) note nil nil nil
                                (mh/annotate-test--uuid (or age 0)))
    (annotate-save-annotations)
    (current-buffer)))

(defun mh/annotate-test--close (file)
  (kill-buffer (find-buffer-visiting file)))

(defun mh/annotate-test--edit (file function)
  "Change FILE on disk behind Emacs's back, as an agent does."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (funcall function)
    (write-region nil nil file nil 'silent)))

(defun mh/annotate-test--db-notes ()
  (cl-loop for record in (annotate-load-annotation-data t)
           append (mapcar (lambda (a) (nth 2 a)) (annotate-annotations-from-dump record))))

(defun mh/annotate-test--summary ()
  (mh/annotate--refresh)
  (mapcar (lambda (it) (list (plist-get it :note) (mh/annotate--lines it)
                             (plist-get it :status) (plist-get it :send)))
          mh/annotate--items))

(defconst mh/annotate-test--python
  "import os\n\ndef load(path):\n    data = open(path).read()\n    return data.split()\n\ndef main():\n    print(load(\"x\"))\n")

;;; annotate.el, adjusted

(ert-deftest mh/annotate-test-own-keys-only ()
  "annotate's map no longer shadows org's and markdown's keys."
  (should (null (cdr annotate-mode-map)))
  (should (eq (lookup-key mh/annotate-map "a") #'mh/annotate-dwim))
  (should (eq (lookup-key mh/annotate-map "s") #'mh/annotate-review)))

(ert-deftest mh/annotate-test-time-round-trip ()
  (should (< (abs (- (float-time) 3600 (mh/annotate--time (mh/annotate-test--uuid 3600)))) 1))
  ;; and against the ids annotate itself writes
  (should (< (abs (- (float-time) (mh/annotate--time (annotate--generate-unique-id)))) 1)))

(ert-deftest mh/annotate-test-anchor ()
  (with-temp-buffer
    (insert "set -e\nfoo\nSET -E\nbar\nset -e\n")
    (should (equal (mh/annotate--anchor 1 7 "set -e") '(1 7 ok 1)))
    ;; nearest of two, and case matters: offset 12 is inside SET -E
    (should (equal (mh/annotate--anchor 12 18 "set -e") '(1 7 moved 2)))
    (should (equal (mh/annotate--anchor 20 26 "set -e") '(23 29 moved 2)))
    (should (eq (nth 2 (mh/annotate--anchor 1 4 "gone")) 'lost))
    (should (eq (nth 2 (mh/annotate--anchor 900 906 "bar")) 'moved))))

(ert-deftest mh/annotate-test-symlinked-file-is-one-file ()
  "A file visited through a symlink is keyed by its real name and listed once."
  (mh/annotate-test--sandbox
    (let* ((file (mh/annotate-test--file "real/a.py" mh/annotate-test--python))
           (link (expand-file-name "today" mh/annotate-test--dir)))
      (make-symbolic-link (file-name-directory file) link)
      (mh/annotate-test--note (expand-file-name "a.py" link) "import os" "unused")
      (should (equal (mapcar #'car (annotate-load-annotation-data t))
                     (list (abbreviate-file-name file))))
      (should (equal (mh/annotate-test--summary) '(("unused" "1" ok t)))))))

(ert-deftest mh/annotate-test-moved-note-survives-reopening ()
  "Six lines above a note are four more than annotate searches."
  (mh/annotate-test--sandbox
    (let ((file (mh/annotate-test--file "a.py" mh/annotate-test--python)))
      (mh/annotate-test--note file "data = open(path).read()" "leaks the fd")
      (mh/annotate-test--close file)
      (mh/annotate-test--edit file (lambda () (insert "# 1\n# 2\n# 3\n# 4\n# 5\n# 6\n")))
      (should (equal (mh/annotate-test--summary) '(("leaks the fd" "10" moved t))))
      (with-current-buffer (find-file-noselect file)
        (mh/annotate-auto-enable)
        (should annotate-mode)
        (should (equal (mh/annotate-test--summary) '(("leaks the fd" "10" ok t)))))
      (mh/annotate-test--close file)
      (should (equal (mh/annotate-test--db-notes) '("leaks the fd")))
      (should (equal (mh/annotate-test--summary) '(("leaks the fd" "10" ok t)))))))

(ert-deftest mh/annotate-test-auto-enable ()
  (mh/annotate-test--sandbox
    (let ((noted (mh/annotate-test--file "noted.py" mh/annotate-test--python))
          (lost (mh/annotate-test--file "lost.py" mh/annotate-test--python))
          (plain (mh/annotate-test--file "plain.py" mh/annotate-test--python)))
      (mh/annotate-test--note noted "import os" "unused")
      (mh/annotate-test--note lost "import os" "unused too")
      (mh/annotate-test--close noted)
      (mh/annotate-test--close lost)
      (mh/annotate-test--edit lost (lambda () (search-forward "import os") (replace-match "import sys")))
      (with-current-buffer (find-file-noselect noted)
        (mh/annotate-auto-enable)
        (should annotate-mode))
      (with-current-buffer (find-file-noselect plain)
        (mh/annotate-auto-enable)
        (should-not annotate-mode))
      ;; annotate would drop this note at the next save; it has to stay readable
      (with-current-buffer (find-file-noselect lost)
        (mh/annotate-auto-enable)
        (should-not annotate-mode))
      (mh/annotate-test--close lost)
      (should (member "unused too" (mh/annotate-test--db-notes)))
      (should (member '("unused too" "?" lost nil) (mh/annotate-test--summary))))))

(ert-deftest mh/annotate-test-lighter ()
  (mh/annotate-test--sandbox
    (let ((a (mh/annotate-test--file "a.py" mh/annotate-test--python))
          (b (mh/annotate-test--file "b.py" mh/annotate-test--python)))
      (mh/annotate-test--note a "import os" "one")
      (mh/annotate-test--note a "main" "two")
      (with-current-buffer (mh/annotate-test--note b "main" "three")
        (should (equal (mh/annotate--lighter) " Ann:1/3"))))))

(ert-deftest mh/annotate-test-dwim-takes-the-line ()
  "Between words annotate would quote the single character at point."
  (mh/annotate-test--sandbox
    (let ((file (mh/annotate-test--file "a.py" mh/annotate-test--python)))
      (with-current-buffer (find-file-noselect file)
        (transient-mark-mode 1)
        (cl-letf (((symbol-function 'read-from-minibuffer) (lambda (&rest _) "a note")))
          (goto-char (point-min))
          (search-forward "data =")
          (mh/annotate-dwim)
          (goto-char (point-min))
          (search-forward "impo")
          (mh/annotate-dwim)
          (goto-char (point-min))
          (forward-line 1)
          (should-error (mh/annotate-dwim) :type 'user-error)
          ;; a whole-line note would replace the one on "import", unasked
          (goto-char (point-min))
          (search-forward "import")
          (should-error (mh/annotate-dwim) :type 'user-error)))
      (mh/annotate--refresh)
      (should (equal (mapcar (lambda (it) (plist-get it :text)) mh/annotate--items)
                     '("import" "data = open(path).read()"))))))

;;; Collecting and choosing

(ert-deftest mh/annotate-test-collect-open-and-closed ()
  (mh/annotate-test--sandbox
    (let ((open (mh/annotate-test--file "open.py" mh/annotate-test--python))
          (closed (mh/annotate-test--file "closed.py" mh/annotate-test--python)))
      (mh/annotate-test--note open "data = open(path).read()" "leaks\nuse with" 60)
      (mh/annotate-test--note closed "data = open(path).read()\n    return data.split()" "two lines" 30)
      (mh/annotate-test--close closed)
      (should (equal (mh/annotate-test--summary)
                     '(("two lines" "4-5" ok t) ("leaks\nuse with" "4" ok t))))
      (should (equal (plist-get (car mh/annotate--items) :text)
                     "data = open(path).read()\n    return data.split()")))))

(ert-deftest mh/annotate-test-latest-run-and-marks ()
  (mh/annotate-test--sandbox
    (let ((file (mh/annotate-test--file "a.py" mh/annotate-test--python)))
      (mh/annotate-test--note file "import os" "old" (* 4 86400))
      (mh/annotate-test--note file "load(path)" "older than the gap" (+ 7300 600))
      (mh/annotate-test--note file "data = open" "recent" 600)
      (mh/annotate-test--note file "main" "new" 0)
      (should (equal (mapcar #'last (mh/annotate-test--summary)) '((nil) (nil) (t) (t))))
      (puthash (plist-get (nth 0 mh/annotate--items) :key) 'send mh/annotate--marks)
      (puthash (plist-get (nth 3 mh/annotate--items) :key) 'hold mh/annotate--marks)
      (should (equal (mapcar #'last (mh/annotate-test--summary)) '((t) (nil) (t) (nil))))
      ;; what was there at the last take was left behind on purpose
      (clrhash mh/annotate--marks)
      (write-region "" nil (mh/annotate--archive-file) nil 'silent)
      (set-file-times (mh/annotate--archive-file) (time-subtract nil 300))
      (should (equal (mapcar #'last (mh/annotate-test--summary)) '((nil) (nil) (nil) (t)))))))

(ert-deftest mh/annotate-test-gone-file ()
  (mh/annotate-test--sandbox
    (let ((file (mh/annotate-test--file "a.py" mh/annotate-test--python)))
      (mh/annotate-test--note file "import os" "unused")
      (mh/annotate-test--close file)
      (delete-file file)
      (should (equal (mh/annotate-test--summary) '(("unused" "?" gone nil)))))))

;;; The prompt and taking

(ert-deftest mh/annotate-test-prompt ()
  (mh/annotate-test--sandbox
    (let ((file (mh/annotate-test--file "a.py" mh/annotate-test--python))
          (old (mh/annotate-test--file "old.py" mh/annotate-test--python)))
      (mh/annotate-test--note file "data = open(path).read()\n    return data.split()" "leaks\nuse with")
      (mh/annotate-test--note old "import os" "stale" (* 4 86400))
      (mh/annotate-set-general "Needs tests.")
      (let (text)
        (mh/annotate--take nil t (lambda (prompt) (setq text prompt)))
        (should (equal text
                       (concat mh/annotate--preamble
                               "# Review feedback\n\n## general\n\nNeeds tests.\n\n## "
                               (abbreviate-file-name file) ":4-5\n\n"
                               "> data = open(path).read()\n>     return data.split()\n\n"
                               "leaks\nuse with\n"
                               "\n---\nHeld back in Emacs, not part of this review: "
                               "1 note(s) in 1 file(s), the oldest written 4d ago.\n")))))))

(ert-deftest mh/annotate-test-prompt-says-when-the-anchor-is-a-guess ()
  "The note was on what is now line 5; nearest to its old offset is line 2."
  (mh/annotate-test--sandbox
    (let ((file (mh/annotate-test--file "a.sh" "set -e\necho a\n")))
      (mh/annotate-test--note file "set -e" "and -u")
      (mh/annotate-test--close file)
      (mh/annotate-test--edit file (lambda () (insert "# x\nset -e\n# y\n# z\n")))
      (mh/annotate--refresh)
      (should (string-suffix-p ":2 (the quoted text occurs 2 times; this is the nearest)"
                               (mh/annotate--heading (car mh/annotate--items)))))))

(ert-deftest mh/annotate-test-take ()
  "Taken notes leave buffers and database; held ones and their file stay."
  (mh/annotate-test--sandbox
    (let ((open (mh/annotate-test--file "open.py" mh/annotate-test--python))
          (closed (mh/annotate-test--file "closed.py" mh/annotate-test--python))
          (old (mh/annotate-test--file "old.py" mh/annotate-test--python))
          (out (expand-file-name "out" mh/annotate-test--dir)))
      (with-current-buffer (mh/annotate-test--note
                            open "data = open(path).read()\n    return data.split()" "two lines")
        (annotate--make-reply-annotation (car (annotate-all-annotations)) "me" "a reply"))
      (mh/annotate-test--note closed "import os" "closed one")
      (mh/annotate-test--close closed)
      (mh/annotate-test--note old "import os" "stale" (* 4 86400))
      (mh/annotate-set-general "Needs tests.")
      (should (= (length (mh/annotate-test--db-notes)) 4))
      (should (= (mh/annotate-take-to-file out) 2))
      (with-temp-buffer
        (insert-file-contents out)
        (dolist (part '("## general\n\nNeeds tests." "\n\ntwo lines\n" "\n\nclosed one\n"
                        "Held back in Emacs"))
          (goto-char (point-min))
          (should (search-forward part nil t)))
        (goto-char (point-min))
        (should-not (search-forward "\n\nstale\n" nil t)))
      (should (equal (mh/annotate-test--db-notes) '("stale")))
      (with-current-buffer (find-buffer-visiting open)
        (should (null (annotate-all-annotations)))
        (should-not (text-property-not-all (point-min) (point-max) 'display nil)))
      (should (equal (mh/annotate-general) ""))
      (should (equal (mh/annotate-test--summary) '(("stale" "1" ok nil))))
      (with-temp-buffer
        (insert-file-contents (mh/annotate--archive-file))
        (should (search-forward "two lines" nil t))))))

(ert-deftest mh/annotate-test-take-the-last-note ()
  "annotate asks before it deletes an emptied database; take must not."
  (mh/annotate-test--sandbox
    (let ((file (mh/annotate-test--file "a.py" mh/annotate-test--python))
          (out (expand-file-name "out" mh/annotate-test--dir)))
      (mh/annotate-test--note file "import os" "only one")
      (should (= (mh/annotate-take-to-file out) 1))
      (should (null (mh/annotate-test--db-notes)))
      (should (= (mh/annotate-take-to-file out) 0))
      (should (equal (with-temp-buffer (insert-file-contents out) (buffer-string))
                     "No annotations to send.\n")))))

(ert-deftest mh/annotate-test-take-all-and-peek ()
  (mh/annotate-test--sandbox
    (let ((file (mh/annotate-test--file "a.py" mh/annotate-test--python))
          (out (expand-file-name "out" mh/annotate-test--dir)))
      (mh/annotate-test--note file "import os" "stale" (* 4 86400))
      (mh/annotate-test--note file "main" "new")
      (should (= (mh/annotate-take-to-file out nil t) 1))
      (should (= (length (mh/annotate-test--db-notes)) 2))
      (should-not (file-exists-p (mh/annotate--archive-file)))
      (should (= (mh/annotate-take-to-file out t) 2))
      (should (null (mh/annotate-test--db-notes))))))

(ert-deftest mh/annotate-test-failed-delivery-loses-nothing ()
  (mh/annotate-test--sandbox
    (let ((file (mh/annotate-test--file "a.py" mh/annotate-test--python)))
      (mh/annotate-test--note file "import os" "keep me")
      (should-error (mh/annotate--take nil nil (lambda (_) (error "Disk full"))))
      (should (equal (mh/annotate-test--db-notes) '("keep me")))
      (should-not (file-exists-p (mh/annotate--archive-file))))))

;;; The review buffer

(defun mh/annotate-test--goto (string)
  (goto-char (point-min))
  (search-forward string)
  (beginning-of-line))

(defun mh/annotate-test--groups ()
  "The review as (SENT-NOTES HELD-NOTES), read off the buffer."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (mapcar (lambda (part)
              (let (notes (start 0))
                (while (string-match "^    [0-9?-]+ +\\(.*?\\)  [0-9a-z]+\\(  .*\\)?$" part start)
                  (push (match-string 1 part) notes)
                  (setq start (match-end 0)))
                (nreverse notes)))
            (list (substring text (string-match "^To send" text) (string-match "^Held back" text))
                  (substring text (string-match "^Held back" text))))))

(ert-deftest mh/annotate-test-review ()
  (mh/annotate-test--sandbox
    (let ((a (mh/annotate-test--file "a.py" mh/annotate-test--python))
          (b (mh/annotate-test--file "b.py" mh/annotate-test--python)))
      (mh/annotate-test--note a "import os" "a one" 20)
      (mh/annotate-test--note a "main" "a two" 10)
      (mh/annotate-test--note b "import os" "b old" (* 4 86400))
      (save-window-excursion
        (mh/annotate-review)
        (should (equal (mh/annotate-test--groups) '(("a one" "a two") ("b old"))))
        (mh/annotate-test--goto "a two")
        (mh/annotate-review-hold)
        (should (equal (mh/annotate-test--groups) '(("a one") ("a two" "b old"))))
        ;; a file heading stands for its notes, a group heading for its files
        (mh/annotate-test--goto "b.py")
        (mh/annotate-review-send)
        (should (equal (mh/annotate-test--groups) '(("a one" "b old") ("a two"))))
        (mh/annotate-test--goto "To send")
        (mh/annotate-review-hold)
        (should (equal (mh/annotate-test--groups) '(nil ("a one" "a two" "b old"))))
        (mh/annotate-review-auto)
        (should (equal (mh/annotate-test--groups) '(("a one" "a two") ("b old"))))
        (mh/annotate-test--goto "General")
        (should-error (mh/annotate-review-send) :type 'user-error)
        (mh/annotate-review-copy)
        (should (string-match-p "a one" (car kill-ring)))
        (should-not (string-match-p "\n\nb old\n" (car kill-ring)))
        (should (equal (mh/annotate-test--groups) '(nil ("b old"))))
        (should-error (mh/annotate-review-copy) :type 'user-error)))))

(provide 'mh-annotate-test)
;;; mh-annotate-test.el ends here
