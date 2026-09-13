(in-package #:doc-extract-protocol)

(defun metadata-plist->object (plist)
  (make-document-metadata
   :title (or (getf plist :title) "")
   :authors (getf plist :authors)
   :dates (getf plist :dates)
   :language (getf plist :language)
   :mimetype (or (getf plist :mimetype)
                 (let ((fmt (getf plist :format)))
                   (when fmt
                     (format nil "~(~a~)" fmt))))
   :filename (getf plist :filename)
   :content-hash (getf plist :content-hash)))

(defun sections->document (section-list &key metadata)
  "Back-compat shim: 0.1 EXTRACTED-SECTION list → EXTRACTED-DOCUMENT.
   Nesting follows heading level."
  (let ((roots '())
        (stack '()))
    (dolist (sec section-list)
      (let* ((level (or (section-level sec) 1))
             (title (or (section-title sec) ""))
             (text (or (section-text sec) ""))
             (block (make-section-block :title title :level level))
             (kids (when (plusp (length (string-trim '(#\Space #\Tab #\Newline)
                                                     text)))
                     (list (make-text-block :text
                                            (string-trim '(#\Space #\Tab #\Newline)
                                                         text))))))
        (setf (section-children block) kids)
        (loop while (and stack (>= (car (first stack)) level))
              do (pop stack))
        (if stack
            (push block (section-children (cdr (first stack))))
            (push block roots))
        (push (cons level block) stack)))
    (labels ((fix (b)
               (when (typep b 'section)
                 (setf (section-children b)
                       (nreverse (section-children b)))
                 (mapc #'fix (section-children b)))))
      (mapc #'fix roots))
    (let ((doc (make-extracted-document
                :metadata (etypecase metadata
                            (null (make-document-metadata))
                            (document-metadata metadata)
                            (list (metadata-plist->object metadata)))
                :blocks (nreverse roots))))
      (ensure-ids doc)
      doc)))

(defgeneric normalize-document (backend raw &key format)
  (:documentation "Normalize backend-specific RAW into EXTRACTED-DOCUMENT."))

(defgeneric extract-document (backend source &key format)
  (:documentation "Primary extraction GF → EXTRACTED-DOCUMENT."))

(defmethod normalize-document ((backend doc-extract-backend) raw &key format)
  (sections->document
   (extract-sections backend raw :format format)
   :metadata (extract-metadata backend raw :format format)))

(defmethod extract-document :around ((backend doc-extract-backend) source
                                     &key format)
  (restart-case (call-next-method)
    (use-value (value)
      :report "Use a supplied extracted-document"
      value)
    (retry ()
      :report "Retry extract-document"
      (extract-document backend source :format format))))

(defmethod extract-document ((backend doc-extract-backend) source &key format)
  (normalize-document backend source :format format))

(defmethod extract-document ((backend null) source &key format)
  (let ((fmt (or format (infer-format source format))))
    (extract-document
     (handler-case (find-extractor fmt)
       (no-extractor-for-format ()
         (%ensure-backend)))
     source :format format)))
