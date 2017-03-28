(in-package :translator-parser)

(defun throw-unexpected (expected-type actual-token)
  (error 'unexpected-token :message "Unexpected token." :expected-type expected-type :actual-token actual-token))

(defmacro pop-bind-tokens ((from-n to-n) &body body)
  `(let* ,(loop for n from from-n to to-n
              collect (let ((tokenn (symbolicate "TOKEN" (write-to-string n))))
                        `(,tokenn (pop *token-list*))))
     ,@body))

(defmacro lookahead-tokens ((from-n to-n) &body body)
  (let ((count 0))
    `(let* ,(loop for n from from-n to to-n
                  collect (let ((tokenn (symbolicate "TOKEN" (write-to-string n))))
                            `(,tokenn ,(prog1 `(nth ,count *token-list*) (incf count)))))
       ,@body)))

(defmacro if-unexpected-throw-unexpected (equality-plist &body body)
  (labels ((%expand-to-if-stmt (plist)
             (let* ((tok (pop plist))
                    (tok-type `(token-type ,tok))
                    (expected (pop plist))
                    (comparison (if (listp expected)
                                    (cons 'or (mapcar (lambda (expected-el)
                                                        `(equalp ,tok-type ,expected-el))
                                                      expected))
                                    `(equalp ,tok-type ,expected)))
                    (expected-to-throw (if (listp expected)
                                           (with-output-to-string (stream)
                                             (format stream "~{~A~^ or ~}" expected))
                                           expected)))
               (if plist
                   `(if ,comparison
                        ,(%expand-to-if-stmt plist)
                        (throw-unexpected ,expected-to-throw ,tok))
                   (if body
                       `(if ,comparison
                            (progn ,@body)
                            (throw-unexpected ,expected-to-throw ,tok))
                       `(unless ,comparison
                          (throw-unexpected ,expected-to-throw ,tok)))))))
    (%expand-to-if-stmt equality-plist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
