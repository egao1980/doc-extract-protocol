(defsystem "doc-extract-protocol"
  :version "0.4.0"
  :description "CLOS document extraction + canonical extracted-document model"
  :author "egao1980"
  :license "MIT"
  :depends-on ("schema-protocol" (:version "markdown-protocol" "0.1.0")
               (:version "encoding-protocol" "0.3.0")
               "crypto-protocol" "crypto-backend-ironclad"
               ;; DOCX / XLSX: zip parts read with compression-protocol (deflate by
               ;; the chipz backend), parsed with xml-protocol (native backend).
               ;; Damaged input signals their own conditions (compression-error,
               ;; xml-error) from these versions on; office.lisp wraps them. 0.5.1 takes
               ;; the last end-of-central-directory record (a stored embedded package).
               (:version "compression-protocol" "0.5.1")
               (:version "compression-backend-chipz" "0.3.0")
               (:version "xml-protocol" "0.1.0") (:version "xml-backend-native" "0.1.1"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "text-decode")
               (:file "model")
               (:file "ids")
               (:file "markup-spans")
               (:file "markup")
               (:file "equivalence")
               (:file "protocol")
               (:file "registry")
               (:file "normalize")
               (:file "html")
               (:file "office"))
  :in-order-to ((test-op (test-op "doc-extract-protocol/tests"))))

(defsystem "doc-extract-protocol/tests"
  :depends-on ("doc-extract-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "protocol-test")
               (:file "decode-test")
               (:file "model-test")
               (:file "ids-test")
               (:file "markup-test")
               (:file "markup-spans-test")
               (:file "normalize-test")
               (:file "registry-test")
               (:file "office-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
