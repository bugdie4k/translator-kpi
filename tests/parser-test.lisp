(in-package :translator-tests)

(deftests test-parser ()
  (correct1
   (translator-parser::program-node-to-list
    (parser (lexer (get-full-test-pathname "test-correct"))))
   '(:PROGRAM-NODE :NAME "TESTCORRECT" :STATEMENTS-LIST
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
  (correct2
   (translator-parser::program-node-to-list
    (parser (lexer (get-full-test-pathname "test-correct2"))))
   '(:PROGRAM-NODE :NAME "TESTCORRECT2" :STATEMENTS-LIST
     ((:LOOP :STATEMENTS-LIST
             ((:FOR :VARIABLE "I" :FROM "1" :TO "2" :STATEMENTS-LIST
                    ((:FOR :VARIABLE "J" :FROM "(+ 1 2)" :TO "I"
                           :STATEMENTS-LIST
                           ((:FOR :VARIABLE "K0" :FROM "A" :TO "(* 2 3)"
                                  :STATEMENTS-LIST NIL)
                            (:FOR :VARIABLE "K1" :FROM "A1" :TO "(* 2 3)"
                                  :STATEMENTS-LIST NIL)
                            (:FOR :VARIABLE "K2" :FROM "A2" :TO "(* 2 3)"
                                  :STATEMENTS-LIST NIL)
                            (:LOOP :STATEMENTS-LIST NIL)))))))
      (:FOR :VARIABLE "K" :FROM "(/ (* L 2) 34)" :TO "(/ 45 M)"
            :STATEMENTS-LIST ((:LOOP :STATEMENTS-LIST NIL)))
      (:LOOP :STATEMENTS-LIST NIL)
      (:FOR :VARIABLE "C" :FROM "C1" :TO "C2" :STATEMENTS-LIST NIL))))
  (correct3
   (translator-parser::program-node-to-list
    (parser (lexer (get-full-test-pathname "test-correct3"))))
   '(:PROGRAM-NODE :NAME "TESTCORRECT3" :STATEMENTS-LIST
     ((:FOR :VARIABLE "A" :FROM "1" :TO "2" :STATEMENTS-LIST
       ((:LOOP :STATEMENTS-LIST NIL)
        (:FOR :VARIABLE "G" :FROM "1" :TO "2" :STATEMENTS-LIST NIL)
        (:FOR :VARIABLE "H" :FROM "3" :TO "4" :STATEMENTS-LIST
              NIL)))
      (:FOR :VARIABLE "B" :FROM "3" :TO "4" :STATEMENTS-LIST NIL)
      (:FOR :VARIABLE "E" :FROM "7" :TO "8" :STATEMENTS-LIST NIL)
      (:FOR :VARIABLE "F" :FROM "9" :TO "10" :STATEMENTS-LIST NIL))))
  (error1
   (handler-case (parser (lexer (get-full-test-pathname "test-parser-error1")))
     (translator-parser::parser-error (e) (print-object e nil)))
   "There is a mallformed expression.
line: 10
column: 17
")
  (error2
   (handler-case (parser (lexer (get-full-test-pathname "test-parser-error2")))
     (translator-parser::parser-error (e) (print-object e nil)))
   "Unexpected token.
expected type: ENDFOR
actual type: USER-DEFINED-IDENTIFIER
       lexem: NOTENDFOR
       line: 12
       column: 10
")
  (error3
   (handler-case (parser (lexer (get-full-test-pathname "test-parser-error3")))
     (translator-parser::parser-error (e) (print-object e nil)))
   "Unexpected token.
expected type: ENDFOR
actual type: ENDLOOP
       lexem: ENDLOOP
       line: 18
       column: 6
")
  (with-labels
      (translator-parser::program-node-to-list
       (parser (lexer (get-full-test-pathname "test-with-labels"))))
    `(:PROGRAM-NODE :NAME "TESTWITHLABELS" :STATEMENTS-LIST
                    ((:FOR :VARIABLE "A" :FROM "1" :TO "2" :STATEMENTS-LIST
                           ((:LOOP :STATEMENTS-LIST
                                   ((:LABELS :LABELS-LIST (1 2 3))
                                    (:LABELS :LABELS-LIST (4 5 6 7))
                                    (:LABELS :LABELS-LIST (8))))
                            (:FOR :VARIABLE "G" :FROM "1" :TO "2" :STATEMENTS-LIST NIL)
                            (:FOR :VARIABLE "H" :FROM "3" :TO "4" :STATEMENTS-LIST
                                  NIL)))
                     (:FOR :VARIABLE "B" :FROM "3" :TO "4" :STATEMENTS-LIST NIL)
                     (:FOR :VARIABLE "E" :FROM "7" :TO "8" :STATEMENTS-LIST NIL)
                     (:FOR :VARIABLE "F" :FROM "9" :TO "10" :STATEMENTS-LIST NIL))))
  (cfg1
   (translator-parser::program-node->cfg-dot
    (parser (lexer (get-full-test-pathname "test-correct"))) nil)
   "digraph program_graph {
    program [label=\"program name: TESTCORRECT\\nBEGIN\"]
    LOOP_STMT1 [label=\"LOOP_STMT1\"]
    program -> LOOP_STMT1 [label=\"\"]
    FOR_STMT2 [label=\"FOR_STMT2\\nfor I\\nfrom (- (* (+ 1 2) (EXPT 3 7)))\\nto (EXPT 1234 (EXPT 3 2))\"]
    LOOP_STMT1 -> FOR_STMT2 [label=\"body\"]
    FOR_STMT3 [label=\"FOR_STMT3\\nfor J\\nfrom (+ 2 3456 (- 1))\\nto I\"]
    FOR_STMT2 -> FOR_STMT3 [label=\"body\"]
    FOR_STMT4 [label=\"FOR_STMT4\\nfor IDENTIFIER0\\nfrom IDENTIFIER1\\nto IDENTIFUER2\"]
    FOR_STMT3 -> FOR_STMT4 [label=\"body\"]
    FOR_STMT4 -> FOR_STMT4 [label=\"body\"]
    FOR_STMT4 -> FOR_STMT3 [label=\"\"]
    FOR_STMT3 -> FOR_STMT2 [label=\"\"]
    FOR_STMT2 -> LOOP_STMT1 [label=\"\"]
    FOR_STMT5 [label=\"FOR_STMT5\\nfor K\\nfrom (- (* (EXPT V0 (EXPT V1 V2)) 321) V3)\\nto (- V1 V2)\"]
    LOOP_STMT1 -> FOR_STMT5 [label=\"\"]
    LOOP_STMT6 [label=\"LOOP_STMT6\"]
    FOR_STMT5 -> LOOP_STMT6 [label=\"body\"]
    LOOP_STMT6 -> LOOP_STMT6 [label=\"body\"]
    LOOP_STMT6 -> FOR_STMT5 [label=\"\"]
    FOR_STMT5 -> END [label=\"\"]
}
")
  (cfg2
   (translator-parser::program-node->cfg-dot (parser (lexer (get-full-test-pathname "test-correct2"))) nil)
   "digraph program_graph {
    program [label=\"program name: TESTCORRECT2\\nBEGIN\"]
    LOOP_STMT1 [label=\"LOOP_STMT1\"]
    program -> LOOP_STMT1 [label=\"\"]
    FOR_STMT2 [label=\"FOR_STMT2\\nfor I\\nfrom 1\\nto 2\"]
    LOOP_STMT1 -> FOR_STMT2 [label=\"body\"]
    FOR_STMT3 [label=\"FOR_STMT3\\nfor J\\nfrom (+ 1 2)\\nto I\"]
    FOR_STMT2 -> FOR_STMT3 [label=\"body\"]
    FOR_STMT4 [label=\"FOR_STMT4\\nfor K0\\nfrom A\\nto (* 2 3)\"]
    FOR_STMT3 -> FOR_STMT4 [label=\"body\"]
    FOR_STMT4 -> FOR_STMT4 [label=\"body\"]
    FOR_STMT5 [label=\"FOR_STMT5\\nfor K1\\nfrom A1\\nto (* 2 3)\"]
    FOR_STMT4 -> FOR_STMT5 [label=\"\"]
    FOR_STMT5 -> FOR_STMT5 [label=\"body\"]
    FOR_STMT6 [label=\"FOR_STMT6\\nfor K2\\nfrom A2\\nto (* 2 3)\"]
    FOR_STMT5 -> FOR_STMT6 [label=\"\"]
    FOR_STMT6 -> FOR_STMT6 [label=\"body\"]
    LOOP_STMT7 [label=\"LOOP_STMT7\"]
    FOR_STMT6 -> LOOP_STMT7 [label=\"\"]
    LOOP_STMT7 -> LOOP_STMT7 [label=\"body\"]
    LOOP_STMT7 -> FOR_STMT3 [label=\"\"]
    FOR_STMT3 -> FOR_STMT2 [label=\"\"]
    FOR_STMT2 -> LOOP_STMT1 [label=\"\"]
    FOR_STMT8 [label=\"FOR_STMT8\\nfor K\\nfrom (/ (* L 2) 34)\\nto (/ 45 M)\"]
    LOOP_STMT1 -> FOR_STMT8 [label=\"\"]
    LOOP_STMT9 [label=\"LOOP_STMT9\"]
    FOR_STMT8 -> LOOP_STMT9 [label=\"body\"]
    LOOP_STMT9 -> LOOP_STMT9 [label=\"body\"]
    LOOP_STMT9 -> FOR_STMT8 [label=\"\"]
    LOOP_STMT10 [label=\"LOOP_STMT10\"]
    FOR_STMT8 -> LOOP_STMT10 [label=\"\"]
    LOOP_STMT10 -> LOOP_STMT10 [label=\"body\"]
    FOR_STMT11 [label=\"FOR_STMT11\\nfor C\\nfrom C1\\nto C2\"]
    LOOP_STMT10 -> FOR_STMT11 [label=\"\"]
    FOR_STMT11 -> FOR_STMT11 [label=\"body\"]
    FOR_STMT11 -> END [label=\"\"]
}
")
  (cfg3
   (translator-parser::program-node->cfg-dot (parser (lexer (get-full-test-pathname "test-correct3"))) nil)
   "digraph program_graph {
    program [label=\"program name: TESTCORRECT3\\nBEGIN\"]
    FOR_STMT1 [label=\"FOR_STMT1\\nfor A\\nfrom 1\\nto 2\"]
    program -> FOR_STMT1 [label=\"\"]
    LOOP_STMT2 [label=\"LOOP_STMT2\"]
    FOR_STMT1 -> LOOP_STMT2 [label=\"body\"]
    LOOP_STMT2 -> LOOP_STMT2 [label=\"body\"]
    FOR_STMT3 [label=\"FOR_STMT3\\nfor G\\nfrom 1\\nto 2\"]
    LOOP_STMT2 -> FOR_STMT3 [label=\"\"]
    FOR_STMT3 -> FOR_STMT3 [label=\"body\"]
    FOR_STMT4 [label=\"FOR_STMT4\\nfor H\\nfrom 3\\nto 4\"]
    FOR_STMT3 -> FOR_STMT4 [label=\"\"]
    FOR_STMT4 -> FOR_STMT4 [label=\"body\"]
    FOR_STMT4 -> FOR_STMT1 [label=\"\"]
    FOR_STMT5 [label=\"FOR_STMT5\\nfor B\\nfrom 3\\nto 4\"]
    FOR_STMT1 -> FOR_STMT5 [label=\"\"]
    FOR_STMT5 -> FOR_STMT5 [label=\"body\"]
    FOR_STMT6 [label=\"FOR_STMT6\\nfor E\\nfrom 7\\nto 8\"]
    FOR_STMT5 -> FOR_STMT6 [label=\"\"]
    FOR_STMT6 -> FOR_STMT6 [label=\"body\"]
    FOR_STMT7 [label=\"FOR_STMT7\\nfor F\\nfrom 9\\nto 10\"]
    FOR_STMT6 -> FOR_STMT7 [label=\"\"]
    FOR_STMT7 -> FOR_STMT7 [label=\"body\"]
    FOR_STMT7 -> END [label=\"\"]
}
")
  (cfg-with-labels
   (translator-parser::program-node->cfg-dot (parser (lexer (get-full-test-pathname "test-with-labels"))) nil)
   "digraph program_graph {
    program [label=\"program name: TESTWITHLABELS\\nBEGIN\"]
    FOR_STMT1 [label=\"FOR_STMT1\\nfor A\\nfrom 1\\nto 2\"]
    program -> FOR_STMT1 [label=\"\"]
    LOOP_STMT2 [label=\"LOOP_STMT2\"]
    FOR_STMT1 -> LOOP_STMT2 [label=\"body\"]
    LABELS_STMT3 [label=\"LABELS_STMT3\\n(1 2 3)\"]
    LOOP_STMT2 -> LABELS_STMT3 [label=\"body\"]
    LABELS_STMT4 [label=\"LABELS_STMT4\\n(4 5 6 7)\"]
    LABELS_STMT3 -> LABELS_STMT4 [label=\"\"]
    LABELS_STMT5 [label=\"LABELS_STMT5\\n(8)\"]
    LABELS_STMT4 -> LABELS_STMT5 [label=\"\"]
    LABELS_STMT5 -> LOOP_STMT2 [label=\"\"]
    FOR_STMT6 [label=\"FOR_STMT6\\nfor G\\nfrom 1\\nto 2\"]
    LOOP_STMT2 -> FOR_STMT6 [label=\"\"]
    FOR_STMT6 -> FOR_STMT6 [label=\"body\"]
    FOR_STMT7 [label=\"FOR_STMT7\\nfor H\\nfrom 3\\nto 4\"]
    FOR_STMT6 -> FOR_STMT7 [label=\"\"]
    FOR_STMT7 -> FOR_STMT7 [label=\"body\"]
    FOR_STMT7 -> FOR_STMT1 [label=\"\"]
    FOR_STMT8 [label=\"FOR_STMT8\\nfor B\\nfrom 3\\nto 4\"]
    FOR_STMT1 -> FOR_STMT8 [label=\"\"]
    FOR_STMT8 -> FOR_STMT8 [label=\"body\"]
    FOR_STMT9 [label=\"FOR_STMT9\\nfor E\\nfrom 7\\nto 8\"]
    FOR_STMT8 -> FOR_STMT9 [label=\"\"]
    FOR_STMT9 -> FOR_STMT9 [label=\"body\"]
    FOR_STMT10 [label=\"FOR_STMT10\\nfor F\\nfrom 9\\nto 10\"]
    FOR_STMT9 -> FOR_STMT10 [label=\"\"]
    FOR_STMT10 -> FOR_STMT10 [label=\"body\"]
    FOR_STMT10 -> END [label=\"\"]
}
"))
