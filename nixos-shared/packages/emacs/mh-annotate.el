;;; mh-annotate.el --- Review annotate.el notes and hand them to an agent  -*- lexical-binding: t; -*-

;;; Commentary:

;; annotate.el notes as an outbox for a coding agent:
;;
;;   C-c a a   annotate (M-RET starts another line)
;;   C-c a s   review them: s and u move notes between "To send" and
;;             "Held back"; c copies the prompt
;;   `annotate-take' in the agent's shell (/mh:notes) fetches the same
;;   prompt without the clipboard
;;
;; Notes that went out are appended to the archive and leave Emacs.  They
;; have to: annotate anchors a note by character offset and searches two
;; lines around it for the quoted text.  The agent's edits move the text
;; further than that, and a note annotate cannot place is gone from the
;; database at its next save.

;;; Code:

(require 'annotate)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit-section)
(require 'pulse)

(defgroup mh/annotate nil
  "Review annotate.el notes and hand them to an agent."
  :group 'annotate)

(defcustom mh/annotate-review-gap (* 2 3600)
  "Seconds of silence between two notes that separate two reviews.
See `mh/annotate--latest-run' for what goes out by default."
  :type 'integer)

(defface mh/annotate-note
  '((((background dark)) :foreground "#EEF192") (t :inherit font-lock-warning-face))
  "A comment in the review buffer.")
(defface mh/annotate-flag '((t :inherit warning))
  "A note whose anchor needs a look.")

(defun mh/annotate--archive-file () (concat (expand-file-name annotate-file) "-sent.md"))
(defun mh/annotate--general-file () (concat (expand-file-name annotate-file) "-general"))

;;; annotate.el, adjusted

;; Its bindings shadow org's and markdown's C-c C-c, C-c C-t, C-c C-s, C-c C-d
;; and C-c C-a wherever the mode is on; `mh/annotate-map' has them instead.
(setcdr annotate-mode-map nil)

(defun mh/annotate--actual-file-name (name)
  "NAME with symlinks resolved, when it names the visited file.
annotate keys its database by the visited name, and ~/Stuff/Today/x is a
different file tomorrow."
  (if (and buffer-file-name (not (file-remote-p buffer-file-name))
           (equal name (abbreviate-file-name buffer-file-name)))
      (abbreviate-file-name (file-truename buffer-file-name))
    name))
(advice-add 'annotate-actual-file-name :filter-return #'mh/annotate--actual-file-name)

(defun mh/annotate--anchor (beg end text)
  "Where TEXT is in the current buffer: (BEG END STATUS MATCHES).
STATUS is `ok' at the stored offsets, `moved' at the occurrence nearest to
them, else `lost'."
  (save-excursion
    (save-restriction
      (widen)
      (cond
       ((or (null text) (string-empty-p text)) (list beg end 'lost 0))
       ((and (integerp beg) (integerp end) (<= (point-min) beg end (point-max))
             (string= (buffer-substring-no-properties beg end) text))
        (list beg end 'ok 1))
       (t
        (let ((case-fold-search nil) (near (or beg 0)) (matches 0) best)
          (goto-char (point-min))
          (while (search-forward text nil t)
            (cl-incf matches)
            (when (or (null best) (< (abs (- (match-beginning 0) near)) (abs (- best near))))
              (setq best (match-beginning 0))))
          (if best
              (list best (+ best (length text)) 'moved matches)
            (list beg end 'lost 0))))))))

(defun mh/annotate--heal (&rest _)
  "Point this file's stored offsets at the quoted text before annotate loads them.
annotate itself gives up two lines away, and what it cannot place it drops."
  (with-demoted-errors "mh/annotate: could not re-anchor: %S"
    (when buffer-file-name
      (let* ((db (annotate-load-annotation-data t))
             (record (assoc-string (annotate-actual-file-name) db))
             changed)
        (dolist (a (annotate-annotations-from-dump record))
          (when (and (car a) (not (annotate-annotation-dump-reply-p a)))
            (pcase-let ((`(,beg ,end ,status) (mh/annotate--anchor (nth 0 a) (nth 1 a) (nth 3 a))))
              (when (eq status 'moved)
                (setf (nth 0 a) beg (nth 1 a) end)
                (setq changed t)))))
        (when changed (annotate-dump-annotation-data db))))))
(advice-add 'annotate-load-annotations :before #'mh/annotate--heal)

(defvar mh/annotate--db-cache nil "(STAMP . RECORDS) of the database, read-only.")

(defun mh/annotate--db ()
  "The database, re-read when the file changed.  Do not modify the result."
  (let* ((attributes (file-attributes (expand-file-name annotate-file)))
         (stamp (and attributes (list (file-attribute-modification-time attributes)
                                      (file-attribute-size attributes)))))
    (unless (and stamp (equal stamp (car mh/annotate--db-cache)))
      (setq mh/annotate--db-cache (cons stamp (and stamp (annotate-load-annotation-data t)))))
    (cdr mh/annotate--db-cache)))

(defun mh/annotate--notes (record)
  (cl-remove-if-not (lambda (a) (and (car a) (not (annotate-annotation-dump-reply-p a))))
                    (annotate-annotations-from-dump record)))

;;;###autoload
(defun mh/annotate-auto-enable ()
  "Turn `annotate-mode' on in a file that has notes.
Not when one of them lost its text: annotate would drop that note at the
next save, and the review is where it can still be read."
  (with-demoted-errors "mh/annotate-auto-enable: %S"
    (when (and buffer-file-name (not (file-remote-p buffer-file-name))
               (not (bound-and-true-p annotate-mode)))
      (when-let* ((record (assoc-string (abbreviate-file-name (file-truename buffer-file-name))
                                        (mh/annotate--db)))
                  (notes (mh/annotate--notes record)))
        (if (cl-some (lambda (a) (eq 'lost (nth 2 (mh/annotate--anchor (nth 0 a) (nth 1 a) (nth 3 a)))))
                     notes)
            (message "A note on this file lost its text; annotate-mode stays off, see the review")
          (annotate-mode 1)
          ;; In a buffer without keywords of its own (text, fundamental)
          ;; font-lock is on in name only at this point, and annotate's
          ;; keywords do not start it: the notes would be there, undrawn.
          (unless (bound-and-true-p jit-lock-mode)
            (font-lock-mode -1)
            (font-lock-mode 1)))))))

(defun mh/annotate--lighter ()
  (format " Ann:%d/%d"
          (cl-count-if #'annotate-chain-first-p (annotate-all-annotations))
          (cl-loop for record in (mh/annotate--db) sum (length (mh/annotate--notes record)))))
;; Notes here / notes anywhere: a second number that surprises is a forgotten note.
(setf (alist-get 'annotate-mode minor-mode-alist) (list '(:eval (mh/annotate--lighter))))

;;; Writing notes

(defvar mh/annotate-note-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-RET") #'newline)
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "C-j") #'newline)
    map)
  "Added to the minibuffer while a note is typed.")

(defmacro mh/annotate--multiline (&rest body)
  "Run BODY; the minibuffer it reads from takes several lines."
  (declare (indent 0))
  `(minibuffer-with-setup-hook
       (lambda () (use-local-map (make-composed-keymap mh/annotate-note-map (current-local-map))))
     ,@body))

(defun mh/annotate-dwim (&optional color)
  "Annotate the region, the symbol at point or the line; edit a note at point.
Turns `annotate-mode' on if need be.  M-RET starts another line, and an
emptied note is deleted.  COLOR is passed on to `annotate-annotate'."
  (interactive "P")
  (unless annotate-mode (annotate-mode 1))
  (let ((annotate-annotation-prompt "Annotation (M-RET: new line): "))
    (mh/annotate--multiline
      (if (or (use-region-p) (annotate-annotation-at (point))
              (annotate-symbol-strictly-at-point))
          (annotate-annotate color)
        ;; annotate would take the one character at point, and a quoted
        ;; space tells the agent nothing
        (save-mark-and-excursion
          (back-to-indentation)
          (when (eolp) (user-error "Nothing to annotate on this line"))
          ;; annotate deletes, unasked, every note a new region encloses
          (when (cl-some #'annotationp (overlays-in (point) (line-end-position)))
            (user-error "This line has a note already: mark a region, or go to a symbol"))
          (set-mark (point))
          (end-of-line)
          (activate-mark)
          (annotate-annotate color))))))

(defun mh/annotate-general ()
  "The comment on the change as a whole, or the empty string."
  (let ((file (mh/annotate--general-file)))
    (if (file-readable-p file)
        (with-temp-buffer (insert-file-contents file) (string-trim (buffer-string)))
      "")))

(defun mh/annotate-set-general (text)
  (let ((file (mh/annotate--general-file)))
    (if (string-empty-p (string-trim text))
        (when (file-exists-p file) (delete-file file))
      (write-region (concat (string-trim text) "\n") nil file nil 'silent))))

(defun mh/annotate-edit-general ()
  "Write the comment on the change as a whole; it goes out first."
  (interactive)
  (mh/annotate-set-general
   (mh/annotate--multiline
     (read-from-minibuffer "General comment (M-RET: new line): " (mh/annotate-general))))
  (mh/annotate--redraw))

;;; Collecting

(defvar mh/annotate--marks (make-hash-table :test #'equal)
  "Choices made in the review, note key -> `send' or `hold'.")
(defvar mh/annotate--items nil "The notes, as plists; see `mh/annotate--collect'.")

(defun mh/annotate--time (id)
  "Creation time of a note, decoded from its UUIDv1 ID."
  (if (and (stringp id) (string-match-p "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-1" id))
      (let ((p (mapcar (lambda (h) (string-to-number h 16)) (split-string id "-"))))
        (/ (- (logior (nth 0 p) (ash (nth 1 p) 32) (ash (logand #xfff (nth 2 p)) 48))
              #x01b21dd213814000)       ; 100ns ticks since 1582 -> unix epoch
           1e7))
    (float-time)))

(defun mh/annotate--age (time)
  (let ((seconds (max 0 (- (float-time) time))))
    (cond ((< seconds 60) "now")
          ((< seconds 3600) (format "%dm" (floor seconds 60)))
          ((< seconds 86400) (format "%dh" (floor seconds 3600)))
          (t (format "%dd" (floor seconds 86400))))))

(defun mh/annotate--snippet (beg end)
  "Buffer text from BEG to END with its syntax colours and nothing else.
annotate draws its note boxes as a `display' property on the newline."
  (ignore-errors (font-lock-ensure beg end))
  (let* ((string (buffer-substring beg end))
         (plain (substring-no-properties string))
         (pos 0))
    (while (< pos (length string))
      (let ((next (or (next-single-property-change pos 'face string) (length string)))
            (face (get-text-property pos 'face string)))
        (when face (put-text-property pos next 'font-lock-face face plain))
        (setq pos next)))
    plain))

(defun mh/annotate--item (file id beg end text note status &rest more)
  (append (list :key (or id (format "%s@%s" file beg)) :id id :file file
                :beg beg :end end :text text :note note :status status
                :time (mh/annotate--time id)
                :line (and (not (memq status '(lost gone))) (line-number-at-pos beg t))
                :line-end (and (not (memq status '(lost gone)))
                               (line-number-at-pos (max beg (1- end)) t)))
          more))

(defun mh/annotate--collect ()
  "Every note there is, as plists.
Live overlays where a buffer has them, so unsaved notes count; the database
for the rest, re-anchored, since its offsets predate later edits."
  (let (items live)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (bound-and-true-p annotate-mode) buffer-file-name
                   (not (buffer-base-buffer buffer)))
          (let ((file (file-truename buffer-file-name)))
            (push file live)
            (dolist (ov (cl-remove-if-not #'annotate-chain-first-p (annotate-all-annotations)))
              (let ((beg (overlay-start ov)) (end (overlay-end (annotate-chain-last ov))))
                (push (mh/annotate--item file (overlay-get ov 'id) beg end
                                         (mh/annotate--snippet beg end)
                                         (overlay-get ov 'annotation) 'ok :buffer buffer)
                      items)))))))
    (dolist (record (mh/annotate--db))
      (let ((name (car record)))
        ;; Info nodes are keyed in here too ("dir")
        (when (and (stringp name) (not (file-remote-p name))
                   (or (file-name-absolute-p name) (string-prefix-p "~" name)))
          (let ((file (file-truename (expand-file-name name))))
            (unless (member file live)
              (if (not (file-readable-p file))
                  (dolist (a (mh/annotate--notes record))
                    (push (mh/annotate--item file (annotate-id-from-dump a) (nth 0 a) (nth 1 a)
                                             (or (nth 3 a) "") (nth 2 a) 'gone)
                          items))
                (with-temp-buffer
                  (insert-file-contents file)
                  (dolist (a (mh/annotate--notes record))
                    (pcase-let ((`(,beg ,end ,status ,matches)
                                 (mh/annotate--anchor (nth 0 a) (nth 1 a) (nth 3 a))))
                      (push (mh/annotate--item file (annotate-id-from-dump a) beg end
                                               (or (nth 3 a) "") (nth 2 a) status
                                               :matches matches)
                            items))))))))))
    ;; Files by their newest note: the review at hand on top, and a note keeps
    ;; its place when its mark changes.  Within a file, reading order.
    (let ((newest (make-hash-table :test #'equal)))
      (dolist (it items)
        (puthash (plist-get it :file)
                 (max (plist-get it :time) (gethash (plist-get it :file) newest 0))
                 newest))
      (sort items (lambda (a b)
                    (let ((fa (plist-get a :file)) (fb (plist-get b :file)))
                      (cond ((string= fa fb) (< (plist-get a :beg) (plist-get b :beg)))
                            ((/= (gethash fa newest) (gethash fb newest))
                             (> (gethash fa newest) (gethash fb newest)))
                            (t (string< fa fb)))))))))

(defun mh/annotate--last-take ()
  "When notes last went out, as the archive remembers it; 0 if never."
  (if-let* ((attributes (file-attributes (mh/annotate--archive-file))))
      (float-time (file-attribute-modification-time attributes))
    0))

(defun mh/annotate--latest-run (items)
  "Keys of the ITEMS that make up the review at hand.
That is the newest run without a gap over `mh/annotate-review-gap', among
the notes written since the last take: what a take left behind was held
back on purpose, and would otherwise become the newest run and go out with
the next one.  A note without its text does not join by itself."
  (let* ((since (mh/annotate--last-take))
         (placed (cl-remove-if (lambda (it) (or (memq (plist-get it :status) '(lost gone))
                                                (<= (plist-get it :time) since)))
                               items))
         (sorted (sort (copy-sequence placed)
                       (lambda (a b) (> (plist-get a :time) (plist-get b :time)))))
         (prev (and sorted (plist-get (car sorted) :time)))
         keys)
    (cl-loop for it in sorted
             for time = (plist-get it :time)
             while (<= (- prev time) mh/annotate-review-gap)
             do (push (plist-get it :key) keys) (setq prev time))
    keys))

(defun mh/annotate--refresh ()
  "Collect the notes and decide which go out."
  (let* ((items (mh/annotate--collect))
         (run (mh/annotate--latest-run items)))
    (setq mh/annotate--items
          (mapcar (lambda (it)
                    (let ((key (plist-get it :key)))
                      (append (list :send (pcase (gethash key mh/annotate--marks)
                                            ('send t) ('hold nil)
                                            (_ (and (member key run) t))))
                              it)))
                  items))))

(defun mh/annotate--sending () (cl-remove-if-not (lambda (it) (plist-get it :send)) mh/annotate--items))
(defun mh/annotate--holding () (cl-remove-if (lambda (it) (plist-get it :send)) mh/annotate--items))

(defun mh/annotate--lines (it)
  (let ((first (plist-get it :line)) (last (plist-get it :line-end)))
    (cond ((null first) "?") ((= first last) (format "%d" first)) (t (format "%d-%d" first last)))))

(defun mh/annotate--flag (it)
  (pcase (plist-get it :status)
    ('moved (if (> (or (plist-get it :matches) 1) 1)
                (format "moved, %d matches" (plist-get it :matches))
              "moved"))
    ('lost "text gone") ('gone "file gone")))

;;; The prompt: marginal's contract, so an agent reads both the same way

(defconst mh/annotate--preamble
  "My review comments from Emacs. Under each `##` heading: the exact text I
selected as a blockquote, then my comment. `## general` is about the change
as a whole. Address every comment.\n\n")

(defun mh/annotate--heading (it)
  (let ((file (abbreviate-file-name (plist-get it :file))))
    (pcase (plist-get it :status)
      ('gone (format "%s (the file is gone)" file))
      ('lost (format "%s (the quoted text is no longer in the file)" file))
      ((and 'moved (guard (> (or (plist-get it :matches) 1) 1)))
       (format "%s:%s (the quoted text occurs %d times; this is the nearest)"
               file (mh/annotate--lines it) (plist-get it :matches)))
      (_ (format "%s:%s" file (mh/annotate--lines it))))))

(defun mh/annotate--prompt (send general held)
  (concat
   (if (and (null send) (string-empty-p general))
       "No annotations to send.\n"
     (concat mh/annotate--preamble "# Review feedback\n\n"
             (if (string-empty-p general) "" (format "## general\n\n%s\n\n" general))
             (mapconcat
              (lambda (it)
                (format "## %s\n\n%s\n\n%s\n"
                        (mh/annotate--heading it)
                        (mapconcat (lambda (line) (concat "> " line))
                                   (split-string (substring-no-properties (plist-get it :text)) "\n")
                                   "\n")
                        (plist-get it :note)))
              send "\n")))
   (if (null held) ""
     (format "\n---\nHeld back in Emacs, not part of this review: %d note(s) in %d file(s), the oldest written %s.\n"
             (length held)
             (length (delete-dups (mapcar (lambda (it) (plist-get it :file)) held)))
             (let ((age (mh/annotate--age (apply #'min (mapcar (lambda (it) (plist-get it :time))
                                                              held)))))
               (if (equal age "now") "just now" (concat age " ago")))))))

;;; Taking notes out

(defun mh/annotate--clear (items)
  "Delete ITEMS and their replies: overlays, leftover display props, database."
  (let ((ids (delq nil (mapcar (lambda (it) (plist-get it :id)) items)))
        (annotate-use-messages nil)
        ;; annotate asks before it deletes a database that became empty, and
        ;; nobody answers a prompt raised through emacsclient
        (annotate-database-confirm-deletion nil))
    (dolist (buffer (delete-dups (delq nil (mapcar (lambda (it) (plist-get it :buffer)) items))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (dolist (first (cl-remove-if-not #'annotate-chain-first-p (annotate-all-annotations)))
            (when (member (overlay-get first 'id) ids)
              (dolist (ov (annotate-find-chain first))
                ;; annotate draws end-of-line notes as a `display' property on
                ;; the newline, and leaves it behind when the overlay goes away.
                (with-silent-modifications
                  (save-excursion
                    (goto-char (overlay-end ov))
                    (end-of-line)
                    (when (< (point) (point-max))
                      (remove-text-properties (point) (1+ (point)) '(display nil)))))
                (delete-overlay ov))))
          (annotate-save-annotations)
          (font-lock-flush))))
    ;; Saving keeps reply records, and closed files were never touched above.
    (annotate-dump-annotation-data
     (cl-loop for record in (annotate-load-annotation-data t)
              for kept = (cl-remove-if (lambda (a) (or (member (annotate-id-from-dump a) ids)
                                                       (member (annotate-reply-to-from-dump a) ids)))
                                       (annotate-annotations-from-dump record))
              when kept collect (list (car record) kept (annotate-checksum-from-dump record)))
     t)))

(defun mh/annotate--take (all peek deliver)
  "Hand the review to DELIVER as a prompt, then move its notes to the archive.
ALL includes the held-back notes; PEEK leaves everything where it is.
DELIVER runs first: if it fails, no note is lost.  Returns the note count."
  (mh/annotate--refresh)
  (let* ((send (if all mh/annotate--items (mh/annotate--sending)))
         (held (if all nil (mh/annotate--holding)))
         (general (mh/annotate-general))
         (text (mh/annotate--prompt send general held)))
    (funcall deliver text)
    (unless (or peek (and (null send) (string-empty-p general)))
      (let ((coding-system-for-write 'utf-8))
        (write-region (format "<!-- taken %s -->\n\n%s\n" (format-time-string "%F %R") text)
                      nil (mh/annotate--archive-file) 'append 'silent))
      (mh/annotate--clear send)
      (mh/annotate-set-general "")
      (dolist (it send) (remhash (plist-get it :key) mh/annotate--marks))
      (mh/annotate--refresh)
      (mh/annotate--redraw))
    (length send)))

(defun mh/annotate-take-to-file (file &optional all peek)
  "Write the review to FILE; for the `annotate-take' script.
emacsclient would print the prompt as a Lisp string."
  (mh/annotate--take all peek
                     (lambda (text)
                       (let ((coding-system-for-write 'utf-8))
                         (write-region text nil file nil 'silent)))))

;;; The review buffer

(defconst mh/annotate--review-buffer "*annotate-review*")

(defun mh/annotate--f (string face) (propertize string 'font-lock-face face))

(defun mh/annotate--display-file (file)
  (let ((name (abbreviate-file-name file)))
    (if (<= (length name) 72) name
      (let ((tail (substring name -70)))
        (concat "…" (substring tail (or (string-search "/" tail) 0)))))))

(defun mh/annotate--insert-note (it)
  (magit-insert-section (mh/annotate-note (plist-get it :key))
    (let* ((note (split-string (plist-get it :note) "\n"))
           (gutter (format "    %-7s" (mh/annotate--lines it)))
           (pad (make-string (length gutter) ?\s))
           (flag (mh/annotate--flag it)))
      (magit-insert-heading
        (concat (mh/annotate--f gutter 'shadow)
                (mh/annotate--f (car note) 'mh/annotate-note)
                (mh/annotate--f (concat "  " (mh/annotate--age (plist-get it :time))) 'shadow)
                (if flag (mh/annotate--f (concat "  " flag) 'mh/annotate-flag) "")))
      (dolist (line (cdr note)) (insert pad (mh/annotate--f line 'mh/annotate-note) "\n"))
      (dolist (line (split-string (plist-get it :text) "\n"))
        (insert pad (mh/annotate--f "│ " 'shadow) line "\n")))))

(defun mh/annotate--insert-group (title kind items empty)
  (magit-insert-section (mh/annotate-group kind)
    (magit-insert-heading (format "%s (%d)" title (length items)))
    (if (null items)
        (insert (mh/annotate--f (format "  %s\n" empty) 'shadow))
      (pcase-dolist (`(,file . ,notes) (seq-group-by (lambda (it) (plist-get it :file)) items))
        (magit-insert-section (mh/annotate-file (cons kind file))
          (magit-insert-heading
            (concat "  " (mh/annotate--f (mh/annotate--display-file file)
                                         'magit-section-secondary-heading)))
          (mapc #'mh/annotate--insert-note notes))))
    (insert "\n")))

(defun mh/annotate--redraw ()
  "Redraw the review from `mh/annotate--items', if it is open."
  (when-let* ((buffer (get-buffer mh/annotate--review-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (line (line-number-at-pos))
            (windows (mapcar (lambda (w) (cons w (line-number-at-pos (window-point w))))
                             (get-buffer-window-list buffer nil t)))
            (general (mh/annotate-general)))
        (erase-buffer)
        (magit-insert-section (mh/annotate-root)
          (magit-insert-section (mh/annotate-general)
            (magit-insert-heading "General")
            (insert (if (string-empty-p general)
                        (mh/annotate--f "  none\n" 'shadow)
                      (concat (replace-regexp-in-string "^" "  " general) "\n"))
                    "\n"))
          (mh/annotate--insert-group "To send" 'send (mh/annotate--sending) "nothing")
          (mh/annotate--insert-group "Held back" 'hold (mh/annotate--holding) "nothing"))
        (goto-char (point-min))
        (forward-line (1- line))
        (pcase-dolist (`(,window . ,at) windows)
          (set-window-point window (save-excursion (goto-char (point-min))
                                                   (forward-line (1- at))
                                                   (point))))))))

(defun mh/annotate--at-point ()
  "The notes point is on: one, a file's, or a group's."
  (let (keys)
    (cl-labels ((walk (section)
                  (pcase (oref section type)
                    ('mh/annotate-note (push (oref section value) keys))
                    ((or 'mh/annotate-file 'mh/annotate-group)
                     (mapc #'walk (oref section children))))))
      (when-let* ((section (magit-current-section))) (walk section)))
    (or (delq nil (mapcar (lambda (key)
                            (cl-find key mh/annotate--items
                                     :key (lambda (it) (plist-get it :key)) :test #'equal))
                          (nreverse keys)))
        (user-error "No note here"))))

(defun mh/annotate--mark (items mark)
  (dolist (it items) (puthash (plist-get it :key) mark mh/annotate--marks))
  (mh/annotate--refresh)
  (mh/annotate--redraw))

(defun mh/annotate-review-send ()
  "Send the note at point; on a file or a group heading, all of its notes."
  (interactive)
  (mh/annotate--mark (mh/annotate--at-point) 'send))

(defun mh/annotate-review-hold ()
  "Hold the note at point back; on a file or a group heading, all of its notes."
  (interactive)
  (mh/annotate--mark (mh/annotate--at-point) 'hold))

(defun mh/annotate-review-send-all () (interactive) (mh/annotate--mark mh/annotate--items 'send))
(defun mh/annotate-review-hold-all () (interactive) (mh/annotate--mark mh/annotate--items 'hold))

(defun mh/annotate-review-auto ()
  "Forget my choices: the latest run goes out, the rest stays."
  (interactive)
  (clrhash mh/annotate--marks)
  (mh/annotate-review-refresh))

(defun mh/annotate-review-refresh ()
  (interactive)
  (mh/annotate--refresh)
  (mh/annotate--redraw))

(defun mh/annotate--visit (it select)
  (let ((file (plist-get it :file)))
    (if (eq (plist-get it :status) 'gone)
        (message "The file is gone: %s" (abbreviate-file-name file))
      (let* ((buffer (or (find-buffer-visiting file) (find-file-noselect file)))
             (window (display-buffer buffer '((display-buffer-reuse-window
                                               display-buffer-use-some-window)
                                              (inhibit-same-window . t)))))
        (when window
          (with-selected-window window
            (let ((beg (min (plist-get it :beg) (point-max)))
                  (end (min (plist-get it :end) (point-max))))
              (goto-char beg)
              (recenter)
              (unless (eq (plist-get it :status) 'lost)
                (pulse-momentary-highlight-region beg end))))
          (when select (select-window window)))))))

(defun mh/annotate-review-visit ()
  "Go to the note at point."
  (interactive)
  (mh/annotate--visit (car (mh/annotate--at-point)) t))

(defun mh/annotate-review-peek ()
  "Show the note at point in another window."
  (interactive)
  (mh/annotate--visit (car (mh/annotate--at-point)) nil))

(defun mh/annotate-review-delete ()
  "Delete the notes at point for good, unsent."
  (interactive)
  (let ((items (mh/annotate--at-point)))
    (when (y-or-n-p (format "Delete %d note(s) without sending? " (length items)))
      (mh/annotate--clear items)
      (mh/annotate-review-refresh))))

(defun mh/annotate-review-copy ()
  "Copy the review as a prompt; its notes move to the archive."
  (interactive)
  (mh/annotate--refresh)
  (when (and (null (mh/annotate--sending)) (string-empty-p (mh/annotate-general)))
    (user-error "Nothing to send"))
  (let ((held (length (mh/annotate--holding))))
    (message "Copied %d note(s) for the agent; %d held back stay."
             (mh/annotate--take nil nil #'kill-new) held)))

(defvar mh/annotate-review-mode-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,key . ,command)
                   '(("s" . mh/annotate-review-send) ("u" . mh/annotate-review-hold)
                     ("S" . mh/annotate-review-send-all) ("U" . mh/annotate-review-hold-all)
                     ("a" . mh/annotate-review-auto) ("g" . mh/annotate-review-refresh)
                     ("RET" . mh/annotate-review-visit) ("o" . mh/annotate-review-peek)
                     ("G" . mh/annotate-edit-general) ("k" . mh/annotate-review-delete)
                     ("c" . mh/annotate-review-copy) ("C-c C-c" . mh/annotate-review-copy)))
      (define-key map (kbd key) command))
    map))

(define-derived-mode mh/annotate-review-mode magit-section-mode "Annotate-Review"
  "The notes that go to the agent, and the ones that stay.
\\<mh/annotate-review-mode-map>\\[mh/annotate-review-send] and \
\\[mh/annotate-review-hold] move the note at point, or all notes of the file
or group whose heading point is on.  \\[mh/annotate-review-copy] copies the
prompt; `annotate-take' in a shell fetches the same.

\\{mh/annotate-review-mode-map}"
  (setq header-line-format
        (substitute-command-keys
         "\\<mh/annotate-review-mode-map> \\[mh/annotate-review-send] send  \
\\[mh/annotate-review-hold] hold  \\[mh/annotate-review-auto] automatic  \
\\[mh/annotate-review-visit] go  \\[mh/annotate-review-peek] peek  \
\\[mh/annotate-edit-general] general  \\[mh/annotate-review-delete] delete  \
\\[mh/annotate-review-copy] copy for the agent  \\[mh/annotate-review-refresh] refresh")))

(defun mh/annotate-review ()
  "Look over the notes before they go to the agent."
  (interactive)
  (mh/annotate--refresh)
  (pop-to-buffer (get-buffer-create mh/annotate--review-buffer))
  (unless (derived-mode-p 'mh/annotate-review-mode) (mh/annotate-review-mode))
  (mh/annotate--redraw)
  (goto-char (point-min)))

;;;###autoload
(defvar mh/annotate-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'mh/annotate-dwim)
    (define-key map "d" #'annotate-delete-annotation)
    (define-key map "n" #'annotate-goto-next-annotation)
    (define-key map "p" #'annotate-goto-previous-annotation)
    (define-key map "g" #'mh/annotate-edit-general)
    (define-key map "s" #'mh/annotate-review)
    (define-key map "m" #'annotate-mode)
    map)
  "Prefix map for annotating; annotate's own map is emptied above.")

(provide 'mh-annotate)
;;; mh-annotate.el ends here
