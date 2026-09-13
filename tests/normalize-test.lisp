(in-package #:doc-extract-protocol/tests)

(defparameter *html*
  "<html><head><title>Doc</title></head><body><h1>Hello</h1><p>world</p><h2>Sub</h2><p>more</p></body></html>")

(defparameter *docx-xml*
  "<?xml version=\"1.0\"?><w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:body><w:p><w:pPr><w:pStyle w:val=\"Heading1\"/></w:pPr><w:r><w:t>Title</w:t></w:r></w:p><w:p><w:r><w:t>Body text</w:t></w:r></w:p></w:body></w:document>")

(deftest html-normalize-document
  (let* ((backend (doc-extract-protocol:make-default-doc-extract-backend))
         (doc (doc-extract-protocol:normalize-document backend *html* :format :html))
         (md (doc-extract-protocol:extracted-document-metadata doc))
         (root (first (doc-extract-protocol:extracted-document-blocks doc))))
    (ok (doc-extract-protocol:extracted-document-p doc))
    (ok (equal "demiurge-doc/1"
               (doc-extract-protocol:extracted-document-schema-version doc)))
    (ok (and md
             (slot-boundp md 'doc-extract-protocol::title)
             (search "Doc" (doc-extract-protocol:document-metadata-title md))))
    (ok (plusp (length (doc-extract-protocol:extracted-document-blocks doc))))
    (ok (search "Hello" (doc-extract-protocol:section-title root)))))

(deftest html-extract-document
  (let ((doc (doc-extract-protocol:extract-document
              (doc-extract-protocol:make-default-doc-extract-backend)
              *html* :format :html)))
    (ok (doc-extract-protocol:extracted-document-p doc))
    (ok (search "world" (doc-extract-protocol:document-text doc)))))

(deftest docx-xml-normalize
  (let ((doc (doc-extract-protocol:extract-document
              (doc-extract-protocol:make-default-doc-extract-backend)
              *docx-xml* :format :docx-xml)))
    (ok (doc-extract-protocol:extracted-document-p doc))
    (ok (search "Title"
                (doc-extract-protocol:section-title
                 (first (doc-extract-protocol:extracted-document-blocks doc)))))
    (ok (search "Body text" (doc-extract-protocol:document-text doc)))))

(deftest sections-shim
  (let* ((secs (list (doc-extract-protocol:make-extracted-section
                      :title "A" :text "aaa" :level 1)
                     (doc-extract-protocol:make-extracted-section
                      :title "B" :text "bbb" :level 2)
                     (doc-extract-protocol:make-extracted-section
                      :title "C" :text "ccc" :level 1)))
         (doc (doc-extract-protocol:sections->document secs
                                                      :metadata '(:title "T"))))
    (ok (doc-extract-protocol:extracted-document-p doc))
    (ok (= 2 (length (doc-extract-protocol:extracted-document-blocks doc))))
    (ok (equal "A" (doc-extract-protocol:section-title
                    (first (doc-extract-protocol:extracted-document-blocks doc)))))
    (let ((a (first (doc-extract-protocol:extracted-document-blocks doc))))
      (ok (find-if (lambda (c) (typep c 'doc-extract-protocol:section))
                   (doc-extract-protocol:section-children a))))))
