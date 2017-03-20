(defsystem :translator
  :description "Lexer and parser for KPI."
  :version "0.0.0"
  :author "Danylo Fedorov <fedorough@gmail.com>"
  :depends-on ("alexandria")
  :serial t
  :components ((:file "package")
               (:module src
                :serial t
                :components ((:module utils
                              :components ((:file "translator-utils")))
                             (:module common
                              :components ((:file "translator-common")))
                             (:module lexer
                              :serial t
                              :components ((:file "lexer-utils")
                                           (:file "lexer")))
                             (:module parser
                              :components ((:file "parser")))))
               (:module tests
                :components ((:file "test-utils")
                             (:file "lexer-test")
                             (:file "parser-test")))))
