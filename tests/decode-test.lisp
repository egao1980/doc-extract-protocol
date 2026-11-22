(in-package #:doc-extract-protocol/tests)

;;; Octet and pathname sources decode as UTF-8 (BOM- and charset-aware).

(defun %u8 (s) (encoding-protocol:encode s :encoding :utf-8))

(defun %cat (&rest parts)
  (apply #'concatenate '(vector (unsigned-byte 8))
         (mapcar (lambda (p) (if (stringp p) (%u8 p) (coerce p '(vector (unsigned-byte 8)))))
                 parts)))

(deftest octet-html-source-is-utf-8
  (let ((text (doc-extract-protocol:extract-text
               (%b) (%u8 "<html><body><p>Ж café 日本 😀</p></body></html>") :format :html)))
    (ok (search "Ж café 日本 😀" text))))

(deftest octet-source-bom-and-declared-charset
  (ok (string= "é" (doc-extract-protocol:decode-source-octets (%cat '(#xEF #xBB #xBF) "é"))))
  (ok (string= "é" (doc-extract-protocol:decode-source-octets
                    (coerce #(#xFF #xFE #xE9 #x00) '(vector (unsigned-byte 8))))))
  (let ((latin (%cat "<meta charset=\"iso-8859-1\"><p>" '(#xE9) "</p>")))
    (ok (search "<p>é</p>" (doc-extract-protocol:decode-source-octets latin))))
  (ok (equal "ISO-8859-1"
             (doc-extract-protocol:declared-charset
              (%u8 "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><a/>"))))
  (ok (string= "é" (let ((doc-extract-protocol:*source-encoding* :iso-8859-1))
                     (doc-extract-protocol:decode-source-octets
                      (coerce #(#xE9) '(vector (unsigned-byte 8))))))))

(deftest octet-source-invalid-utf-8
  (testing "declared UTF-8 that is invalid signals doc-extract-error"
    (ok (signals (doc-extract-protocol:decode-source-octets
                  (%cat "<meta charset=utf-8>" '(#xC0 #xAF)))
                 'doc-extract-protocol:doc-extract-error)))
  (testing "undeclared invalid UTF-8 falls back to ISO-8859-1 with a warning"
    (let ((warned nil))
      (ok (string= "é"
                   (handler-bind ((warning (lambda (w) (setf warned t) (muffle-warning w))))
                     (doc-extract-protocol:decode-source-octets
                      (coerce #(#xE9) '(vector (unsigned-byte 8)))))))
      (ok warned))))

(deftest pathname-source-is-utf-8
  (uiop:with-temporary-file (:pathname p :type "html" :element-type '(unsigned-byte 8) :stream s)
    (write-sequence (%u8 "<html><body><h1>Привет</h1></body></html>") s)
    (finish-output s)
    :close-stream
    (ok (search "Привет" (doc-extract-protocol:extract-text (%b) p :format :html)))))

(deftest block-ids-stable
  (ok (string= "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
               (doc-extract-protocol:portable-digest "")))
  (ok (string= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
               (doc-extract-protocol:portable-digest "abc"))))
