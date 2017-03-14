(defsystem :translator
  :description "Lexer and parser for KPI."
  :version "0.0.0"
  :author "Danylo Fedorov <fedorough@gmail.com>"
  :depends-on ("alexandria" "lisp-unit2")
  :serial t
  :components ((:file "package")
               (:module utils
                :components ((:file "translator-utils")))
               (:module common
                :components ((:file "translator-common")))
               (:module src
                :serial t
                :components ((:file "lexer")
                             (:file "parser")))
               (:module tests
                :components ((:file "lexer-test")
                             (:file "parser-test")))))
