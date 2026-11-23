(defsystem "doc-extract-protocol"
  :version "0.3.1"
  :description "CLOS document extraction + canonical extracted-document model"
  :author "egao1980"
  :license "MIT"
  :depends-on ("schema-protocol" (:version "markdown-protocol" "0.1.0")
               (:version "encoding-protocol" "0.3.0")
               "crypto-protocol" "crypto-backend-ironclad")
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
  ;; compression-backend-chipz: zip reader for the DOCX / XLSX fixtures.
  :depends-on ("doc-extract-protocol" "compression-backend-chipz" "rove")
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
