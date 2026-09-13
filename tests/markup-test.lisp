(in-package #:doc-extract-protocol/tests)

(deftest markup-sample-strict
  (let* ((doc (make-sample-doc :unicode-p t :title "Markup"))
         (md (doc-extract-protocol:dump-markup doc :profile :archival))
         (round (doc-extract-protocol:parse-markup md)))
    (ok (search "<!--" md))
    (ok (search "café" md))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :text))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :structure))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :strict))))

(deftest markup-llm-keeps-ids
  (let* ((doc (make-sample-doc :title "LLM"))
         (md (doc-extract-protocol:dump-markup doc :profile :llm))
         (round (doc-extract-protocol:parse-markup md)))
    (ng (search "page=" md))
    (ok (search "{#" md))
    (ok (equal (doc-extract-protocol:block-id
                (first (doc-extract-protocol:extracted-document-blocks doc)))
               (doc-extract-protocol:block-id
                (first (doc-extract-protocol:extracted-document-blocks round)))))))

(deftest markup-generated-docs
  (let ((failed 0)
        (n 32))
    (dotimes (i n)
      (let* ((doc (gen-document (+ 1000 (* i 17))))
             (md (doc-extract-protocol:dump-markup doc :profile :archival))
             (round (doc-extract-protocol:parse-markup md)))
        (unless (and (doc-extract-protocol:document-equivalent-p
                      doc round :level :text)
                     (doc-extract-protocol:document-equivalent-p
                      doc round :level :structure)
                     (doc-extract-protocol:document-equivalent-p
                      doc round :level :strict))
          (incf failed))))
    (ok (zerop failed)
        (format nil "markup property: ~d/~d generated docs failed" failed n))))
