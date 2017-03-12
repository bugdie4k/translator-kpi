(defsystem :translator
  :description "Lexer and parser for KPI."
  :version 0.0.0
  :author "Danylo Fedorov <fedorough@gmail.com>"
  :components ((:file "package")
               (:modile src
                :serial t
                :components ((:file "lexer")
                             (:file "parser")))))
