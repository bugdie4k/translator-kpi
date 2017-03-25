(defsystem :translator
  :description "Lexer and parser for KPI."
  :version "0.0.0"
  :author "Danylo Fedorov <fedorough@gmail.com>"
  :depends-on ("alexandria" "small-tests")
  :serial t
  :components ((:file "package")
               (:module src
                :serial t
                :components ((:module common
                              :components ((:file "translator-common")))
                             (:module lexer
                              :serial t
                              :components ((:file "lexer-utils")
                                           (:file "lexer")))
                             (:module parser
                              :serial t
                              :components ((:file "parser")
                                           (:file "parser-utils")
                                           (:file "expression-parser")))))
               (:module tests
                :components ((:file "lexer-test")
                             (:file "parser-test")))))
