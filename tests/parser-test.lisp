(in-package :translator-tests)

(defun expected-list ()
  '(:PROGRAM-NODE :NAME "TEST01" :STATEMENTS-LIST
    ((:LOOP :STATEMENTS-LIST
            ((:FOR :VARIABLE "I" :FROM "(- (* (+ 1 2) (EXPT 3 7)))" :TO
                   "(EXPT 1234 (EXPT 3 2))" :STATEMENTS-LIST
                   ((:FOR :VARIABLE "J" :FROM "(+ 2 3456 (- 1))" :TO "I"
                          :STATEMENTS-LIST
                          ((:FOR :VARIABLE "IDENTIFIER0" :FROM "IDENTIFIER1" :TO
                                 "IDENTIFUER2" :STATEMENTS-LIST NIL)))))))
     (:FOR :VARIABLE "K" :FROM
      "(- (* (EXPT V0 (EXPT V1 V2)) 321) V3)" :TO "(- V1 V2)"
      :STATEMENTS-LIST ((:LOOP :STATEMENTS-LIST NIL))))))

(deftests test-parser ()
  (correct 
   (translator-parser::program-node-to-list
    (parser (lexer "/home/danylo/kpi/3/translators-marchenko/translator/tests/testfiles/test-correct")))
   (expected-list))
  (error1
   (handler-case (parser (lexer "/home/danylo/kpi/3/translators-marchenko/translator/tests/testfiles/test-parser-error1"))
     (translator-parser::parser-error (e) (print-object e nil)))
   "There is a mallformed expression.
line: 10
column: 17
")
  (error2
   (handler-case (parser (lexer "/home/danylo/kpi/3/translators-marchenko/translator/tests/testfiles/test-parser-error2"))
     (translator-parser::parser-error (e) (print-object e nil)))
   "Unexpected token.
expected type: ENDFOR
actual type: USER-DEFINED-IDENTIFIER
       lexem: NOTENDFOR
       line: 12
       column: 10
")
  (error3
   (handler-case (parser (lexer "/home/danylo/kpi/3/translators-marchenko/translator/tests/testfiles/test-parser-error3"))
     (translator-parser::parser-error (e) (print-object e nil)))
   "Unexpected token.
expected type: ENDFOR
actual type: ENDLOOP
       lexem: ENDLOOP
       line: 18
       column: 6
"))
