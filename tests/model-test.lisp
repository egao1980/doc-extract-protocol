(in-package #:doc-extract-protocol/tests)

(deftest schema-dump-parse-lossless
  (let* ((doc (make-sample-doc :unicode-p t))
         (ht (doc-extract-protocol:dump-document doc))
         (round (doc-extract-protocol:parse-extracted-document ht)))
    (ok (hash-table-p ht))
    (ok (doc-extract-protocol:extracted-document-p round))
    (ok (equal doc-extract-protocol:+document-schema-version+
               (doc-extract-protocol:extracted-document-schema-version round)))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :strict))))

(deftest sexp-print-read
  (let* ((doc (make-sample-doc :title "Sexp"))
         (round (sexp-roundtrip doc)))
    (ok (doc-extract-protocol:extracted-document-p round))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :strict))))

(deftest document-text-derived
  (let ((doc (make-sample-doc :unicode-p t)))
    (multiple-value-bind (text spans)
        (doc-extract-protocol:document-text doc :layer :body)
      (ok (search "café" text))
      (ok (search "日本語" text))
      (ng (search "Page 1" text))
      (ok (hash-table-p spans))
      (let* ((body (first (doc-extract-protocol:section-children
                           (first (doc-extract-protocol:extracted-document-blocks doc)))))
             (id (doc-extract-protocol:block-id body))
             (span (gethash id spans)))
        (ok span)
        (ok (= (length "café 日本語 naïve")
               (- (second span) (first span))))))
    (let ((all (doc-extract-protocol:document-text doc :layer :all)))
      (ok (search "Page 1" all)))))

(deftest find-block-index
  (let* ((doc (make-sample-doc))
         (id (doc-extract-protocol:block-id
              (first (doc-extract-protocol:extracted-document-blocks doc)))))
    (ok (typep (doc-extract-protocol:find-block doc id)
               'doc-extract-protocol:section))))

(deftest split-block-tree-metadata
  (let* ((doc (make-sample-doc))
         (chunks (doc-extract-protocol:split-block-tree doc)))
    (ok (plusp (length chunks)))
    (ok (every (lambda (c) (getf c :block-id)) chunks))
    (ok (every (lambda (c) (getf c :section-path)) chunks))
    (ng (find "Page 1" chunks :key (lambda (c) (getf c :text)) :test #'search))
    (ok (find :table chunks :key (lambda (c) (getf c :kind))))))

(deftest render-table-markdown
  (let* ((doc (make-sample-doc))
         (table (find-if (lambda (b) (typep b 'doc-extract-protocol:table-block))
                         (doc-extract-protocol:block-children
                          (first (doc-extract-protocol:extracted-document-blocks doc))))))
    (ok table)
    (let ((md (doc-extract-protocol:render-table table :format :markdown)))
      (ok (search "H1" md))
      (ok (search "---" md)))))

(deftest equivalence-tiers
  (let ((a (make-sample-doc :title "A"))
        (b (make-sample-doc :title "A")))
    (ok (doc-extract-protocol:document-equivalent-p a b :level :text))
    (ok (doc-extract-protocol:document-equivalent-p a b :level :structure))
    (ok (doc-extract-protocol:document-equivalent-p a b :level :strict)))
  (let ((a (make-sample-doc :title "A"))
        (b (make-sample-doc :title "B")))
    (ok (doc-extract-protocol:document-equivalent-p a b :level :text))
    (ok (doc-extract-protocol:document-equivalent-p a b :level :structure))
    (let ((kid (first (doc-extract-protocol:section-children
                       (first (doc-extract-protocol:extracted-document-blocks b))))))
      (when (typep kid 'doc-extract-protocol:text-block)
        (setf (doc-extract-protocol:block-id kid) "mutated"))
      (ng (doc-extract-protocol:document-equivalent-p a b :level :strict)))))
