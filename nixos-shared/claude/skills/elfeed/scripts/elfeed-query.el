;;; elfeed-query.el --- Query elfeed database programmatically

;; Helper functions to query elfeed database from command line via emacsclient

(require 'elfeed)
(require 'elfeed-db)
(require 'elfeed-search)
(require 'json)

(defun elfeed-query-entries (filter-string &optional format limit)
  "Query elfeed database with FILTER-STRING.
FORMAT can be 'sexp (default), 'json, or 'simple.
LIMIT is max number of results (nil for all matching entries)."
  (elfeed-db-ensure)
  (let* ((filter (elfeed-search-parse-filter filter-string))
         (entries '())
         (count 0)
         (effective-limit (or limit most-positive-fixnum)))
    ;; Collect matching entries
    (with-elfeed-db-visit (entry feed)
      (when (and (< count effective-limit)
                 (elfeed-search-filter filter entry feed count))
        (push (list :entry entry :feed feed) entries)
        (setq count (1+ count))))
    ;; Format results
    (setq entries (nreverse entries))
    (pcase format
      ('json (elfeed-query--format-json entries))
      ('simple (elfeed-query--format-simple entries))
      (_ (elfeed-query--format-sexp entries)))))

(defun elfeed-query--format-sexp (entries)
  "Format ENTRIES as s-expression."
  (mapcar
   (lambda (item)
     (let ((entry (plist-get item :entry))
           (feed (plist-get item :feed)))
       `(:title ,(elfeed-entry-title entry)
         :link ,(elfeed-entry-link entry)
         :date ,(elfeed-entry-date entry)
         :tags ,(elfeed-entry-tags entry)
         :feed-title ,(elfeed-feed-title feed)
         :feed-url ,(elfeed-feed-url feed))))
   entries))

(defun elfeed-query--format-json (entries)
  "Format ENTRIES as JSON string."
  (json-encode
   (mapcar
    (lambda (item)
      (let ((entry (plist-get item :entry))
            (feed (plist-get item :feed)))
        `((title . ,(elfeed-entry-title entry))
          (link . ,(elfeed-entry-link entry))
          (date . ,(elfeed-entry-date entry))
          (tags . ,(vconcat (mapcar #'symbol-name (elfeed-entry-tags entry))))
          (feed_title . ,(elfeed-feed-title feed))
          (feed_url . ,(elfeed-feed-url feed)))))
    entries)))

(defun elfeed-query--format-simple (entries)
  "Format ENTRIES as simple text (one per line)."
  (mapconcat
   (lambda (item)
     (let ((entry (plist-get item :entry))
           (feed (plist-get item :feed)))
       (format "%s | %s | %s | %s"
               (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time (elfeed-entry-date entry)))
               (elfeed-feed-title feed)
               (elfeed-entry-title entry)
               (elfeed-entry-link entry))))
   entries
   "\n"))

(defun elfeed-query-stats ()
  "Return database statistics."
  (elfeed-db-ensure)
  (let ((total-entries 0)
        (unread-count 0)
        (feeds (make-hash-table :test 'equal)))
    (with-elfeed-db-visit (entry feed)
      (setq total-entries (1+ total-entries))
      (when (elfeed-tagged-p 'unread entry)
        (setq unread-count (1+ unread-count)))
      (puthash (elfeed-feed-url feed) t feeds))
    `(:total-entries ,total-entries
      :unread-entries ,unread-count
      :total-feeds ,(hash-table-count feeds)
      :last-update ,(elfeed-db-last-update))))

(defun elfeed-query-feeds ()
  "Return list of all feeds."
  (elfeed-db-ensure)
  (let ((feeds (make-hash-table :test 'equal))
        (results '()))
    (with-elfeed-db-visit (entry feed)
      (let ((url (elfeed-feed-url feed)))
        (unless (gethash url feeds)
          (puthash url t feeds)
          (push `(:title ,(elfeed-feed-title feed)
                  :url ,url
                  :author ,(elfeed-feed-author feed))
                results))))
    (nreverse results)))

(defun elfeed-query-tags ()
  "Return list of all tags in the database."
  (elfeed-db-ensure)
  (elfeed-db-get-all-tags))

(provide 'elfeed-query)
;;; elfeed-query.el ends here
