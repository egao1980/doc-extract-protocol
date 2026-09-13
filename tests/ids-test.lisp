(in-package #:doc-extract-protocol/tests)

(deftest portable-sha256-vectors
  (ok (equal "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
             (doc-extract-protocol:portable-digest "")))
  (ok (equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
             (doc-extract-protocol:portable-digest "abc"))))

(deftest same-content-same-ids
  (let ((a (make-sample-doc :title "Same"))
        (b (make-sample-doc :title "Same")))
    (ok (equal (doc-extract-protocol:block-id
                (first (doc-extract-protocol:extracted-document-blocks a)))
               (doc-extract-protocol:block-id
                (first (doc-extract-protocol:extracted-document-blocks b)))))
    (let ((ids-a '())
          (ids-b '()))
      (doc-extract-protocol::map-document-blocks
       a (lambda (blk) (push (doc-extract-protocol:block-id blk) ids-a)))
      (doc-extract-protocol::map-document-blocks
       b (lambda (blk) (push (doc-extract-protocol:block-id blk) ids-b)))
      (ok (equal (nreverse ids-a) (nreverse ids-b))))))

(deftest ids-survive-schema-roundtrip
  (let* ((doc (make-sample-doc :title "Round"))
         (round (doc-extract-protocol:parse-extracted-document
                 (doc-extract-protocol:dump-document doc)))
         (again (doc-extract-protocol:ensure-ids
                 (doc-extract-protocol:parse-extracted-document
                  (doc-extract-protocol:dump-document doc)))))
    (ok (equal (doc-extract-protocol:block-id
                (first (doc-extract-protocol:extracted-document-blocks doc)))
               (doc-extract-protocol:block-id
                (first (doc-extract-protocol:extracted-document-blocks round)))))
    (ok (equal (doc-extract-protocol:block-id
                (first (doc-extract-protocol:extracted-document-blocks doc)))
               (doc-extract-protocol:block-id
                (first (doc-extract-protocol:extracted-document-blocks again)))))))

(deftest digest-hook
  (let ((doc-extract-protocol:*block-digest-fn*
          (lambda (octets)
            (declare (ignore octets))
            "deadbeef")))
    (ok (equal "deadbeef"
               (doc-extract-protocol:compute-block-id
                (doc-extract-protocol:make-text-block :text "x"))))))
