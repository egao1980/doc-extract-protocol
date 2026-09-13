(in-package #:doc-extract-protocol/tests)

(deftest html-office-self-register
  (ok (typep (doc-extract-protocol:find-extractor :html)
             'doc-extract-protocol:default-doc-extract-backend))
  (ok (typep (doc-extract-protocol:find-extractor "text/html")
             'doc-extract-protocol:default-doc-extract-backend))
  (ok (typep (doc-extract-protocol:find-extractor :docx)
             'doc-extract-protocol:default-doc-extract-backend))
  (ok (typep (doc-extract-protocol:find-extractor :xlsx-xml)
             'doc-extract-protocol:default-doc-extract-backend)))

(deftest no-extractor-signals
  (ok (signals (doc-extract-protocol:find-extractor :pdf)
               'doc-extract-protocol:no-extractor-for-format)))

(deftest use-backend-restart
  (let ((sentinel (doc-extract-protocol:make-default-doc-extract-backend))
        (got nil))
    (handler-bind ((doc-extract-protocol:no-extractor-for-format
                    (lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'doc-extract-protocol:use-backend sentinel))))
      (setf got (doc-extract-protocol:find-extractor :unknown-format)))
    (ok (eq sentinel got))))

(deftest canonicalize-format
  (ok (eq :html (doc-extract-protocol:canonicalize-format "HTML")))
  (ok (eq :html (doc-extract-protocol:canonicalize-format "file.HTM")))
  (ok (eq :docx (doc-extract-protocol:canonicalize-format
                 "application/vnd.openxmlformats-officedocument.wordprocessingml.document")))
  (ok (eq :pdf (doc-extract-protocol:canonicalize-format "doc.pdf"))))

(defclass %low-extractor (doc-extract-protocol:doc-extract-backend) ())
(defclass %high-extractor (doc-extract-protocol:doc-extract-backend) ())

(deftest register-priority
  (let ((doc-extract-protocol:*extractors* nil)
        (low (make-instance '%low-extractor))
        (high (make-instance '%high-extractor)))
    (doc-extract-protocol:register-extractor low :formats '(:rtf) :priority 1)
    (doc-extract-protocol:register-extractor high :formats '(:rtf) :priority 50)
    (ok (eq high (doc-extract-protocol:find-extractor :rtf)))))
