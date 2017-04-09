(in-package :translator-parser)

(defun parser (token-list)
  (let ((*token-list* token-list)
        (*tree*))
    (parse-program)))

(defun parser-handled (token-list &key (stream t))
  (handler-case (parser token-list)
    (translator-condition (e) (print-object e stream))))

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
                           (:labels (push (parse-labels-statement) statements-list)
                             (%parse-statements-list-aux))
                           (t (reverse statements-list))))))
      (%parse-statements-list-aux))))

(defun parse-labels-statement ()
  (make-instance 'labels-statement-node
                 :labels-list (pop-bind-tokens (1 2)
                                    (if-unexpected-throw-unexpected (token1 :labels
                                                                     token2 :colon)
                                      (prog1 (parse-labels-list)
                                        (pop-bind-tokens (3 3)
                                          (if-unexpected-throw-unexpected (token3 :semicolon))))))))

(defun parse-labels-list ()
  (let ((labels-list))
    (labels ((%parse-labels-list-aux ()
               (lookahead-tokens (1 1)
                 (case (token-type token1)
                   (:number-literal
                    (pop-bind-tokens (2 2)
                      (declare (ignore token2)) ; token1 and token2 are same token1
                      (push (token-lexem token1) labels-list)
                      (%parse-labels-list-aux)))
                   (:comma (pop-bind-tokens (2 2)
                             (declare (ignore token2))
                             (%parse-labels-list-aux)))
                   (t (reverse labels-list))))))
      (%parse-labels-list-aux))))

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
