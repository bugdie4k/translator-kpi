(in-package :translator-tests)

(defun extract-token-essentials (token-list)
  (mapcar (lambda (token) (list (token-string token) (intern (symbol-name (token-type token)) :translator-tests) (token-line token) (token-column token))) token-list))

(defun equal-token-essentials-list (list-list1 list-list2)
  (not (eq 'not-equal (if (eql (length list-list1) (length list-list2))
                          (loop for list1 in list-list1
                                for list2 in list-list2 do
                                  (let ((res0 (if (eql (length list1) (length list2))
                                                  (let ((res (loop for el1 in list1
                                                                   for el2 in list2 do
                                                                     (unless (equalp el1 el2) (return 'not-equal)))))
                                                    (when (eq res 'not-equal) (return 'not-equal)))
                                                  'not-equal)))
                                    (when (eq res0 'not-equal) (return 'not-equal))))
                          'not-equal))))

(deftests test-lexer ((testfiles (pathname "/home/danylo/kpi/3/translators-marchenko/translator/tests/testfiles/")))
  (correct
   (extract-token-essentials (lexer (merge-pathnames testfiles (pathname "./test-correct"))))
   '(("PROGRAM" PROGRAM 1 0)
     ("TEST01" USER-DEFINED-IDENTIFIER 1 8)
     ("BEGIN" BEGIN 2 0)
     ("LOOP" LOOP 3 2)
     ("FOR" FOR 4 4)
     ("I" USER-DEFINED-IDENTIFIER 4 8)
     (":=" ASSIGNMENT 4 10)
     ("-" MINUS-SIGN 4 13)
     ("1" NUMBER 4 14)
     ("TO" TO 4 16)
     ("12" NUMBER 4 19)
     ("DO" DO 4 22)
     ("FOR" FOR 5 8)
     ("J" USER-DEFINED-IDENTIFIER 5 12)
     (":=" ASSIGNMENT 5 14)
     ("2" NUMBER 5 17)
     ("+" PLUS-SIGN 5 18)
     ("3456" NUMBER 5 19)
     ("-" MINUS-SIGN 5 23)
     ("1" NUMBER 5 24)
     ("TO" TO 5 26)
     ("I" USER-DEFINED-IDENTIFIER 5 29)
     ("DO" DO 5 31)
     ("FOR" FOR 6 10)
     ("IDENTIFIER0" USER-DEFINED-IDENTIFIER 6 14)
     (":=" ASSIGNMENT 6 26)
     ("IDENTIFIER1" USER-DEFINED-IDENTIFIER 6 29)
     ("TO" TO 6 41)
     ("IDENTIFUER2" USER-DEFINED-IDENTIFIER 6 44)
     ("DO" DO 6 56)
     ("ENDFOR" ENDFOR 6 59)
     ("ENDFOR" ENDFOR 7 8)
     ("ENDFOR" ENDFOR 8 4)
     ("ENDLOOP" ENDLOOP 9 2))
   #'equal-token-essentials-list)
  (error-one-char
   (handler-case (lexer (merge-pathnames testfiles (pathname "./test-lexer-error-one-char")))
     (wrong-character (e) (print-object e nil)))
   "This character is not allowed.
character: &
line: 3
column: 3
")
  (error-assignment-operator
   (handler-case (lexer (merge-pathnames testfiles (pathname "./test-lexer-error-assignment-operator")))
     (wrong-character (e) (print-object e nil)))
   "Assignment operator is supposed because of colon character. Second character must be an equals sign.
character: 3
line: 4
column: 11
")
  (error-empty-file
   (handler-case (lexer (merge-pathnames testfiles (pathname "./test-lexer-error-empty-file")))
     (empty-file (e) (print-object e nil)))
   "This file is empty
file: /home/danylo/kpi/3/translators-marchenko/translator/tests/testfiles/test-lexer-error-empty-file
"))
