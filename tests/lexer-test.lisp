(in-package :translator-tests)

(defun extract-token-essentials (token-list)
  (mapcar (lambda (token) (list (token-lexem token) (token-type token) (token-line token) (token-column token))) token-list))

(defun get-full-test-pathname (testname)
  (merge-pathnames "/home/danylo/kpi/3/translators-marchenko/translator/tests/testfiles/" (pathname testname)))

(deftests test-lexer ()
  (correct
   (extract-token-essentials (lexer (get-full-test-pathname "test-correct")))
   '(("PROGRAM" :PROGRAM 1 0)
     ("TEST01" :USER-DEFINED-IDENTIFIER 1 8) (";" :SEMICOLON 1 14)
     ("BEGIN" :BEGIN 3 0) ("LOOP" :LOOP 3 2) ("FOR" :FOR 4 4)
     ("I" :USER-DEFINED-IDENTIFIER 4 8) (":=" :ASSIGNMENT 4 10)
     ("-" :MINUS 4 13) ("(" :LEFT-PARENTHESIS 4 14)
     (1 :NUMBER-LITERAL 4 14) ("+" :PLUS 4 17)
     (2 :NUMBER-LITERAL 4 19) (")" :RIGHT-PARENTHESIS 4 20)
     ("*" :MULTIPLICATION 4 22) (3 :NUMBER-LITERAL 4 24)
     ("^" :POWER 4 25) (7 :NUMBER-LITERAL 4 26) ("TO" :TO 4 28)
     (1234 :NUMBER-LITERAL 4 31) ("^" :POWER 4 35)
     ("(" :LEFT-PARENTHESIS 4 36) (3 :NUMBER-LITERAL 4 36)
     ("^" :POWER 4 38) (2 :NUMBER-LITERAL 4 39)
     (")" :RIGHT-PARENTHESIS 4 40) ("DO" :DO 5 42) ("FOR" :FOR 10 8)
     ("J" :USER-DEFINED-IDENTIFIER 10 12) (":=" :ASSIGNMENT 10 14)
     (2 :NUMBER-LITERAL 10 17) ("+" :PLUS 10 18)
     (3456 :NUMBER-LITERAL 10 19) ("-" :MINUS 10 23)
     (1 :NUMBER-LITERAL 10 24) ("TO" :TO 10 26)
     ("I" :USER-DEFINED-IDENTIFIER 10 29) ("DO" :DO 11 31)
     ("FOR" :FOR 11 10)
     ("IDENTIFIER0" :USER-DEFINED-IDENTIFIER 11 14)
     (":=" :ASSIGNMENT 11 26)
     ("IDENTIFIER1" :USER-DEFINED-IDENTIFIER 11 29) ("TO" :TO 11 41)
     ("IDENTIFUER2" :USER-DEFINED-IDENTIFIER 11 44) ("DO" :DO 12 56)
     ("ENDFOR" :ENDFOR 12 10) (";" :SEMICOLON 12 16)
     ("ENDFOR" :ENDFOR 13 8) (";" :SEMICOLON 13 14)
     ("ENDFOR" :ENDFOR 14 4) (";" :SEMICOLON 14 10)
     ("ENDLOOP" :ENDLOOP 15 2) (";" :SEMICOLON 15 9)
     ("FOR" :FOR 16 2) ("K" :USER-DEFINED-IDENTIFIER 16 6)
     (":=" :ASSIGNMENT 16 8) ("V0" :USER-DEFINED-IDENTIFIER 16 11)
     ("^" :POWER 16 13) ("V1" :USER-DEFINED-IDENTIFIER 16 14)
     ("^" :POWER 16 16) ("V2" :USER-DEFINED-IDENTIFIER 16 17)
     ("*" :MULTIPLICATION 16 19) (321 :NUMBER-LITERAL 16 20)
     ("-" :MINUS 16 23) ("V3" :USER-DEFINED-IDENTIFIER 16 24)
     ("TO" :TO 16 27) ("V1" :USER-DEFINED-IDENTIFIER 16 30)
     ("-" :MINUS 16 32) ("V2" :USER-DEFINED-IDENTIFIER 16 33)
     ("DO" :DO 17 36) ("LOOP" :LOOP 18 6) ("ENDLOOP" :ENDLOOP 18 6)
     (";" :SEMICOLON 18 13) ("ENDFOR" :ENDFOR 19 2)
     (";" :SEMICOLON 19 8) ("END" :END 20 0) ("." :FULL-STOP 20 3)))
  (error-one-char
   (handler-case (lexer (get-full-test-pathname "test-lexer-error-one-char"))
     (translator-lexer::wrong-character (e) (print-object e nil)))
   "This character is not allowed.
character: &
line: 3
column: 3
")
  (error-assignment-operator
   (handler-case (lexer (get-full-test-pathname "test-lexer-error-assignment-operator"))
     (translator-lexer::wrong-character (e) (print-object e nil)))
   "Assignment operator is expected because of colon character. Second character must be an equals sign: =.
character: 3
line: 4
column: 11
")
  (error-empty-file
   (handler-case (lexer (get-full-test-pathname "test-lexer-error-empty-file"))
     (translator-lexer::empty-file (e) (print-object e nil)))
   "This file is empty.
file: /home/danylo/kpi/3/translators-marchenko/translator/tests/testfiles/test-lexer-error-empty-file
")
  (error-unexpected-eof1
   (handler-case (lexer (get-full-test-pathname "test-lexer-error-unexpected-eof1"))
     (translator-lexer::wrong-character (e) (print-object e nil)))
   "Unexpected end of file at the following position.
character: EOF
line: 15
column: 4
")
  (error-unexpected-eof2
   (handler-case (lexer (get-full-test-pathname "test-lexer-error-unexpected-eof2"))
     (translator-lexer::wrong-character (e) (print-object e nil)))
   "Unexpected end of file at the following position.
character: EOF
line: 14
column: 10
"))
