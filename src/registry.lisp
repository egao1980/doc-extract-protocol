(in-package #:doc-extract-protocol)

(defvar *extractors* nil
  "Registered extractors: list of (backend formats priority) plists.")

(defun canonicalize-format (format)
  "Keyword for a mimetype, extension, pathname, or format keyword."
  (cond
    ((null format) nil)
    ((pathnamep format)
     (canonicalize-format (or (pathname-type format) (namestring format))))
    ((keywordp format)
     (case format
       ((:htm) :html)
       ((:docx-xml) :docx-xml)
       ((:xlsx-xml) :xlsx-xml)
       (t format)))
    ((stringp format)
     (let* ((s (string-downcase (string-trim '(#\Space) format)))
            (slash (position #\/ s))
            (dot (position #\. s :from-end t))
            (ext (cond
                   (slash nil)
                   (dot (subseq s (1+ dot)))
                   (t s))))
       (cond
         ((or (string= s "text/html")
              (string= s "application/xhtml+xml")
              (string= ext "html")
              (string= ext "htm"))
          :html)
         ((or (string= s "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
              (string= ext "docx"))
          :docx)
         ((or (string= s "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
              (string= ext "xlsx"))
          :xlsx)
         ((string= ext "docx-xml") :docx-xml)
         ((string= ext "xlsx-xml") :xlsx-xml)
         ((or (string= s "application/pdf") (string= ext "pdf"))
          :pdf)
         ((and ext (plusp (length ext)))
          (intern (string-upcase ext) :keyword))
         (t
          (intern (string-upcase s) :keyword)))))
    (t format)))

(defun %entry-backend (entry) (getf entry :backend))
(defun %entry-formats (entry) (getf entry :formats))
(defun %entry-priority (entry) (or (getf entry :priority) 0))

(defun %backend-key (backend)
  (typecase backend
    (symbol backend)
    (doc-extract-backend (class-name (class-of backend)))
    (t backend)))

(defun register-extractor (backend &key formats (priority 0))
  "Register BACKEND (class name or instance) for FORMATS at PRIORITY.
   Higher priority wins in FIND-EXTRACTOR."
  (check-type formats list)
  (let* ((canon (remove nil (mapcar #'canonicalize-format formats)))
         (key (%backend-key backend))
         (existing (find key *extractors* :key (lambda (e)
                                                 (%backend-key (%entry-backend e)))
                         :test #'equal)))
    (cond
      (existing
       (setf (getf existing :formats)
             (remove-duplicates (append (%entry-formats existing) canon)
                                :test #'eq)
             (getf existing :priority) priority
             (getf existing :backend) backend)
       existing)
      (t
       (let ((entry (list :backend backend :formats canon :priority priority)))
         (push entry *extractors*)
         entry)))))

(defun %entry-matches (entry format)
  (let ((fmt (canonicalize-format format)))
    (and fmt (member fmt (%entry-formats entry) :test #'eq))))

(defun %instantiate-backend (backend)
  (typecase backend
    (doc-extract-backend backend)
    (symbol
     (if (and *doc-extract-backend* (typep *doc-extract-backend* backend))
         *doc-extract-backend*
         (make-instance backend)))
    (t backend)))

(defun find-extractor (format &key (registry *extractors*))
  "Highest-priority registered backend for FORMAT.
   Signals NO-EXTRACTOR-FOR-FORMAT with USE-BACKEND restart."
  (let* ((matches (remove-if-not (lambda (e) (%entry-matches e format))
                                 registry))
         (best (and matches
                    (first (sort (copy-list matches) #'>
                                 :key #'%entry-priority)))))
    (if best
        (%instantiate-backend (%entry-backend best))
        (restart-case
            (error 'no-extractor-for-format
                   :format format
                   :message (format nil "no extractor for ~s" format))
          (use-backend (backend)
            :report "Use a supplied extractor backend"
            :interactive (lambda ()
                           (format *query-io* "Backend: ")
                           (force-output *query-io*)
                           (list (read *query-io*)))
            backend)))))
