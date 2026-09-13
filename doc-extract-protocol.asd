(defsystem "doc-extract-protocol"
  :version "0.2.0"
  :description "CLOS document extraction + canonical extracted-document model"
  :author "egao1980"
  :license "MIT"
  :depends-on ("schema-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "model")
               (:file "ids")
               (:file "markup")
               (:file "equivalence")
               (:file "registry")
               (:file "protocol")
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
               (:file "model-test")
               (:file "ids-test")
               (:file "markup-test")
               (:file "normalize-test")
               (:file "registry-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
