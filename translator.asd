(defsystem :translator
  :description "Lexer and parser for KPI."
  :version "0.0.0"
  :author "Danylo Fedorov <fedorough@gmail.com>"
  :depends-on ("alexandria" "cmu-infix" "parse-number" "small-tests" "my-cl-utils")
  :serial t
  :components ((:file "packages")
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
                              :components ((:file "parser-utils")
                                           (:file "expression-parser")
                                           (:file "parser")))))
               (:module tests
                :components ((:file "lexer-test")
                             (:file "parser-test")
                             (:file "toplevel-test")))))
