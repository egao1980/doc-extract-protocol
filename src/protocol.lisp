(in-package #:doc-extract-protocol)

(defclass doc-extract-backend () ())

(defclass default-doc-extract-backend (doc-extract-backend) ())

(defvar *doc-extract-backend* nil)

(defun make-default-doc-extract-backend ()
  (make-instance 'default-doc-extract-backend))

(defun use-default-doc-extract-backend ()
  (setf *doc-extract-backend* (make-default-doc-extract-backend)))

(defclass extracted-section ()
  ((title :initarg :title :accessor section-title :initform "")
   (text :initarg :text :accessor section-text :initform "")
   (level :initarg :level :accessor section-level :initform 1)))

(defun extracted-section-p (x)
  (typep x 'extracted-section))

(defun make-extracted-section (&key (title "") (text "") (level 1))
  (make-instance 'extracted-section :title title :text text :level level))

(defun html-backend-bound-p ()
  (let ((pkg (find-package :html-protocol)))
    (and pkg
         (let ((v (find-symbol "*HTML-BACKEND*" pkg)))
           (and v (boundp v) (symbol-value v))))))

(defun xml-backend-bound-p ()
  (let ((pkg (find-package :xml-protocol)))
    (and pkg
         (let ((v (find-symbol "*XML-BACKEND*" pkg)))
           (and v (boundp v) (symbol-value v))))))

(defun zip-available-p ()
  (let ((pkg (find-package :compression-protocol)))
    (and pkg
         (let ((open (find-symbol "OPEN-ARCHIVE" pkg)))
           (and open (fboundp open))))))

(defun infer-format (source format)
  (or format
      (when (or (pathnamep source) (and (stringp source)
                                        (position #\. source :from-end t)
                                        (or (search ".htm" source :test #'char-equal)
                                            (search ".html" source :test #'char-equal)
                                            (search ".docx" source :test #'char-equal)
                                            (search ".xlsx" source :test #'char-equal)
                                            (search ".pdf" source :test #'char-equal))))
        (let* ((name (if (pathnamep source)
                         (namestring source)
                         source))
               (dot (position #\. name :from-end t)))
          (when dot
            (let ((ext (string-downcase (subseq name (1+ dot)))))
              (cond
                ((member ext '("html" "htm") :test #'string=) :html)
                ((string= ext "docx") :docx)
                ((string= ext "xlsx") :xlsx)
                ((string= ext "pdf") :pdf)
                (t nil))))))
      (when (and (stringp source)
                 (or (search "<html" source :test #'char-equal)
                     (search "<!doctype html" source :test #'char-equal)))
        :html)
      :html))

(defun %source-string (source)
  (etypecase source
    (string source)
    (pathname (with-open-file (s source :direction :input)
                (let ((buf (make-string (file-length s))))
                  (read-sequence buf s)
                  buf)))
    ((vector (unsigned-byte 8))
     (map 'string #'code-char source))))

(defgeneric extract-text (backend source &key format)
  (:documentation "Plain text of SOURCE. FORMAT :html :docx :xlsx :pdf
   :docx-xml :xlsx-xml."))

(defgeneric extract-metadata (backend source &key format)
  (:documentation "Plist of metadata (title, creator, …)."))

(defgeneric extract-sections (backend source &key format)
  (:documentation "List of EXTRACTED-SECTION (title, text, level)."))

(defun %ensure-backend (&optional (backend *doc-extract-backend*))
  (or backend
      (setf *doc-extract-backend* (make-default-doc-extract-backend))))

(defmethod extract-text :around ((backend doc-extract-backend) source
                                 &key format)
  (restart-case (call-next-method)
    (use-value (value)
      :report "Use a supplied extracted string"
      value)
    (retry ()
      :report "Retry extract-text"
      (extract-text backend source :format format))))

(defmethod extract-text ((backend default-doc-extract-backend) source
                         &key format)
  (let ((fmt (infer-format source format)))
    (ecase fmt
      (:html (html-extract-text source))
      ((:docx :xlsx :docx-xml :xlsx-xml)
       (office-extract-text source :format fmt))
      (:pdf
       (error 'doc-extract-unsupported :format :pdf
              :message "PDF extraction requires doc-extract-backend-pdf (pdfium follow-up)")))))

(defmethod extract-metadata ((backend default-doc-extract-backend) source
                             &key format)
  (let ((fmt (infer-format source format)))
    (ecase fmt
      (:html (html-extract-metadata source))
      ((:docx :xlsx :docx-xml :xlsx-xml)
       (office-extract-metadata source :format fmt))
      (:pdf
       (error 'doc-extract-unsupported :format :pdf
              :message "PDF extraction requires doc-extract-backend-pdf")))))

(defmethod extract-sections ((backend default-doc-extract-backend) source
                             &key format)
  (let ((fmt (infer-format source format)))
    (ecase fmt
      (:html (html-extract-sections source))
      ((:docx :xlsx :docx-xml :xlsx-xml)
       (office-extract-sections source :format fmt))
      (:pdf
       (error 'doc-extract-unsupported :format :pdf
              :message "PDF extraction requires doc-extract-backend-pdf")))))

(defmethod extract-text ((backend null) source &key format)
  (extract-text (%ensure-backend) source :format format))

(defmethod extract-metadata ((backend null) source &key format)
  (extract-metadata (%ensure-backend) source :format format))

(defmethod extract-sections ((backend null) source &key format)
  (extract-sections (%ensure-backend) source :format format))
