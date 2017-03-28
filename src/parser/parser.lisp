(in-package :translator-parser)

(defun parser (token-list)
  (let ((*token-list* token-list)
        (*tree*))
    (parse-program)))

(defun parse-program ()
  (pop-bind-tokens (1 3)
    (if-unexpected-throw-unexpected (token1 :program
                                     token2 :user-defined-identifier
                                     token3 :semicolon)
      (make-instance 'program-node
                     :program-name (token-lexem token2)
                     :statements-list (parse-block)))))

(defun parse-block ()  
  (pop-bind-tokens (1 1)
    (if-unexpected-throw-unexpected (token1 :begin)
      (prog1 (parse-statements-list)
        (pop-bind-tokens (2 3)
          (if-unexpected-throw-unexpected (token2 :end
                                           token3 :full-stop)))))))

(defun parse-statements-list ()
  (let ((statements-list))
    (labels ((%parse-statements-list-aux ()
               (lookahead-tokens (1 1)
                         (case (token-type token1)
                           (:loop (push (parse-loop-statement) statements-list)
                                 (%parse-statements-list-aux))
                           (:for (push (parse-for-statement) statements-list)
                            (%parse-statements-list-aux))
                           (t (reverse statements-list))))))
      (%parse-statements-list-aux))))

(defun parse-loop-statement ()
  (make-instance 'loop-statement-node
                 :statements-list (pop-bind-tokens (1 1)
                                    (if-unexpected-throw-unexpected (token1 :loop)
                                      (prog1 (parse-statements-list)
                                        (pop-bind-tokens (1 2)
                                          (if-unexpected-throw-unexpected (token1 :endloop
                                                                           token2 :semicolon))))))))

(defun parse-for-statement ()
  (make-instance 'for-statement-node
                 :variable (pop-bind-tokens (1 3)
                             (if-unexpected-throw-unexpected (token1 :for
                                                              token2 :user-defined-identifier
                                                              token3 :assignment)
                               (token-lexem token2)))
                 :from-expression (prog1 (write-to-string (infix-tokens->prefix-sexp))
                                    (pop-bind-tokens (1 1)
                                      (if-unexpected-throw-unexpected (token1 :to))))
                 :to-expression (prog1 (write-to-string (infix-tokens->prefix-sexp))
                                  (pop-bind-tokens (1 1)
                                    (if-unexpected-throw-unexpected (token1 :do))))
                 :statements-list (prog1 (parse-statements-list)
                                   (pop-bind-tokens (1 2)
                                     (if-unexpected-throw-unexpected (token1 :endfor
                                                                      token2 :semicolon))))))
