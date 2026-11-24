(in-package #:doc-extract-protocol)

(define-condition doc-extract-error (error)
  ((message :initarg :message :reader doc-extract-error-message :initform nil)
   (format :initarg :format :reader doc-extract-error-format :initform nil)
   (cause :initarg :cause :reader doc-extract-error-cause :initform nil))
  (:report (lambda (c s)
             (format s "doc-extract error~@[ (~a)~]~@[: ~a~]"
                     (doc-extract-error-format c)
                     (doc-extract-error-message c))))
  (:documentation "Root of doc-extract-protocol's conditions: a source that
cannot be extracted. CAUSE is the stack condition it wraps, when there is
one (an xml-protocol XML-ERROR for a malformed part, a compression-protocol
COMPRESSION-ERROR for a damaged package)."))

(define-condition doc-extract-unsupported (doc-extract-error)
  ()
  (:report (lambda (c s)
             (format s "doc-extract unsupported~@[ format ~a~]~@[: ~a~]"
                     (doc-extract-error-format c)
                     (doc-extract-error-message c)))))

(define-condition no-extractor-for-format (doc-extract-error)
  ((format :initarg :format :reader no-extractor-for-format-format
           :initform nil))
  (:report (lambda (c s)
             (format s "no extractor registered for format ~s~@[: ~a~]"
                     (no-extractor-for-format-format c)
                     (doc-extract-error-message c)))))
