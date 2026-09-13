(in-package #:doc-extract-protocol/tests)

(defun %b ()
  (doc-extract-protocol:make-default-doc-extract-backend))

(defparameter *html*
  "<html><head><title>Doc</title></head><body><h1>Hello</h1><p>world</p><h2>Sub</h2><p>more</p></body></html>")

(deftest html-extract-text
  (let ((text (doc-extract-protocol:extract-text (%b) *html* :format :html)))
    (ok (search "Hello" text))
    (ok (search "world" text))))

(deftest html-extract-sections
  (let ((secs (doc-extract-protocol:extract-sections (%b) *html* :format :html)))
    (ok (plusp (length secs)))
    (ok (extracted-section-like (first secs)))
    (ok (search "Hello" (doc-extract-protocol:section-title (first secs))))))

(defun extracted-section-like (s)
  (doc-extract-protocol:extracted-section-p s))

(deftest html-extract-metadata
  (let ((md (doc-extract-protocol:extract-metadata (%b) *html* :format :html)))
    (ok (eq :html (getf md :format)))
    (ok (equal "Doc" (getf md :title)))))

(deftest html-null-backend
  (let ((doc-extract-protocol:*doc-extract-backend* nil)
        (text nil))
    (setf text (doc-extract-protocol:extract-text nil *html* :format :html))
    (ok (search "Hello" text))))

(deftest pdf-unsupported
  (ok (signals (doc-extract-protocol:extract-text (%b) "x" :format :pdf)
               'doc-extract-protocol:doc-extract-unsupported)))

(deftest pdf-use-value
  (let ((got nil))
    (handler-bind ((doc-extract-protocol:doc-extract-unsupported
                    (lambda (c)
                      (use-value "fallback" c))))
      (setf got (doc-extract-protocol:extract-text (%b) "x" :format :pdf)))
    (ok (string= "fallback" got))))

(defparameter *docx-xml*
  "<?xml version=\"1.0\"?><w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:body><w:p><w:pPr><w:pStyle w:val=\"Heading1\"/></w:pPr><w:r><w:t>Title</w:t></w:r></w:p><w:p><w:r><w:t>Body text</w:t></w:r></w:p></w:body></w:document>")

(deftest docx-xml-extract
  (let ((text (doc-extract-protocol:extract-text (%b) *docx-xml* :format :docx-xml)))
    (ok (search "Title" text))
    (ok (search "Body text" text)))
  (let ((secs (doc-extract-protocol:extract-sections (%b) *docx-xml* :format :docx-xml)))
    (ok (plusp (length secs)))
    (ok (search "Title" (doc-extract-protocol:section-title (first secs))))))

(deftest docx-zip-skip-if-unavailable
  (if (doc-extract-protocol:zip-available-p)
      (ok t)
      (skip "compression-protocol zip not loaded")))
