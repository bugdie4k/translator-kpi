(in-package :translator-tests)

(defun extract-token-essentials (token-list)
  (mapcar (lambda (token) (list (token-lexem token) (intern (symbol-name (token-type token)) :translator-tests) (token-line token) (token-column token))) token-list))

(defun equal-token-essentials-list (list-list1 list-list2)
  (not (eq 'not-equal (if (eql (length list-list1) (length list-list2))
                          (loop for list1 in list-list1
                                for list2 in list-list2 do
                                  (let ((res0 (if (eql (length list1) (length list2))
                                                  (let ((res (loop for el1 in list1
                                                                   for el2 in list2 do
                                                                     (unless (equalp el1 el2)
                                                                       (format t "log/equal-token-essentials-list~%~A != ~A~%" el1 el2)
                                                                       (return 'not-equal)))))
                                                    (when (eq res 'not-equal) (return 'not-equal)))
                                                  (progn (format t "log/equal-token-essentials-list~%~A~%!=~%~A~%" list1 list2) 'not-equal))))
                                    (when (eq res0 'not-equal) (return 'not-equal))))
                          (progn (format t "log/equal-token-essentials-list~%~A~%!=~%~A~%" list-list1 list-list2) 'not-equal)))))

(defun get-full-test-pathname (testname)
  (merge-pathnames "/home/danylo/kpi/3/translators-marchenko/translator/tests/testfiles/" (pathname testname)))

(deftests test-lexer ()
  (correct
   (extract-token-essentials (lexer (get-full-test-pathname "test-correct")))
   '(("PROGRAM" PROGRAM 1 0) ("TEST01" USER-DEFINED-IDENTIFIER 1 8)
     (";" SEMICOLON 1 14) ("BEGIN" BEGIN 3 0) ("LOOP" LOOP 3 2)
     ("FOR" FOR 4 4) ("I" USER-DEFINED-IDENTIFIER 4 8)
     (":=" ASSIGNMENT 4 10) ("-" MINUS-SIGN 4 13)
     ("1" NUMBER-LITERAL 4 14) ("TO" TO 4 16)
     ("12" NUMBER-LITERAL 4 19) ("DO" DO 5 22) ("FOR" FOR 10 8)
     ("J" USER-DEFINED-IDENTIFIER 10 12) (":=" ASSIGNMENT 10 14)
     ("2" NUMBER-LITERAL 10 17) ("+" PLUS-SIGN 10 18)
     ("3456" NUMBER-LITERAL 10 19) ("-" MINUS-SIGN 10 23)
     ("1" NUMBER-LITERAL 10 24) ("TO" TO 10 26)
     ("I" USER-DEFINED-IDENTIFIER 10 29) ("DO" DO 11 31)
     ("FOR" FOR 11 10) ("IDENTIFIER0" USER-DEFINED-IDENTIFIER 11 14)
     (":=" ASSIGNMENT 11 26)
     ("IDENTIFIER1" USER-DEFINED-IDENTIFIER 11 29) ("TO" TO 11 41)
     ("IDENTIFUER2" USER-DEFINED-IDENTIFIER 11 44) ("DO" DO 11 56)
     ("ENDFOR" ENDFOR 11 59) (";" SEMICOLON 11 65)
     ("ENDFOR" ENDFOR 12 8) (";" SEMICOLON 12 14)
     ("ENDFOR" ENDFOR 13 4) (";" SEMICOLON 13 10)
     ("ENDLOOP" ENDLOOP 14 2) (";" SEMICOLON 14 9) ("END" END 15 0))
   #'equal-token-essentials-list)
  (error-one-char
   (handler-case (lexer (get-full-test-pathname "test-lexer-error-one-char"))
     (wrong-character (e) (print-object e nil)))
   "This character is not allowed.
character: &
line: 3
column: 3
")
  (error-assignment-operator
   (handler-case (lexer (get-full-test-pathname "test-lexer-error-assignment-operator"))
     (wrong-character (e) (print-object e nil)))
   "Assignment operator is expected because of colon character. Second character must be an equals sign: =.
character: 3
line: 4
column: 11
")
  (error-empty-file
   (handler-case (lexer (get-full-test-pathname "test-lexer-error-empty-file"))
     (empty-file (e) (print-object e nil)))
   "This file is empty.
file: /home/danylo/kpi/3/translators-marchenko/translator/tests/testfiles/test-lexer-error-empty-file
")
  (error-unexpected-eof
   (handler-case (lexer (get-full-test-pathname "test-lexer-error-unexpected-eof"))
     (wrong-character (e) (print-object e nil)))
   "Unexpected end of file after the following position.
character: EOF
line: 15
column: 2
"))
