(in-package :translator-parser)

(defun throw-unexpected (expected actual)
  (error 'unexpected-token :message "Unexpected token." :expected expected :actual actual))

(defmacro pop-bind-tokens ((from-n to-n) &body body)
  `(let* ,(loop for n from from-n to to-n
              append (let ((tokenn (symbolicate "TOKEN" (write-to-string n)))
                           (token-typen (symbolicate "TOKEN" (write-to-string n) "-TYPE")))
                       `((,tokenn (pop *token-list*))
                         (,token-typen (token-type ,tokenn)))))
     ,@body))

(defmacro lookahead-tokens ((from-n to-n) &body body)
  (let ((count 0))
    `(let* ,(loop for n from from-n to to-n
                  append (let ((tokenn (symbolicate "TOKEN" (write-to-string n)))
                               (token-typen (symbolicate "TOKEN" (write-to-string n) "-TYPE")))
                           `((,tokenn ,(prog1 `(nth ,count *token-list*) (incf count)))
                             (,token-typen (token-type ,tokenn)))))
       ,@body)))

(defmacro if-unexpected-throw-unexpected (equality-plist &body body)
  (labels ((%expand-to-if-stmt (plist)
             (let* ((variable (pop plist))
                   (expected (pop plist))
                   (comparison (if (listp expected)
                                   (cons 'or (mapcar (lambda (expected-el)
                                                       `(equalp ,variable ,expected-el))
                                                     expected))
                                   `(equalp ,variable ,expected)))
                    (expected-to-throw (if (listp expected)
                                           (with-output-to-string (stream)
                                             (format stream "~{~A~^ or ~}" expected))
                                           expected)))
               (if plist
                   `(if ,comparison
                        ,(%expand-to-if-stmt plist)
                        (throw-unexpected ,expected-to-throw ,variable))
                   (if body
                       `(if ,comparison
                            (progn ,@body)
                            (throw-unexpected ,expected-to-throw ,variable))
                       `(unless ,comparison
                          (throw-unexpected ,expected-to-throw ,variable)))))))
    (%expand-to-if-stmt equality-plist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parser (token-list)
  (let ((*token-list* token-list)
        (*tree*))
    (parse-program)))

(defun parse-program ()
  (pop-bind-tokens (1 3)
    (if-unexpected-throw-unexpected (token1-type :program
                                     token2-type :identifier
                                     token3-type :semicolon)
      (make-instance 'program-node
                     :name (token-lexem token2)
                     :statements-list (parse-block)))))

(defun parse-block ()  
  (pop-bind-tokens (1 1)
    (if-unexpected-throw-unexpected (token1-type :begin)
      (prog1 (parse-statements-list)
        (pop-bind-tokens (2 3)
          (if-unexpected-throw-unexpected (token2-type :end
                                           token3-type :full-stop)))))))

(defun parse-statements-list ()
  (let ((statements-list))
    (labels ((%parse-statements-list-aux ()
               (pop-bind-tokens (1 1)
                         (case token1-type
                           (:loop (push (parse-loop-statement-rest) statements-list))
                           (:for (push (parse-for-statement-rest) statements-list))))))
      (%parse-statements-list-aux))))

(defun parse-loop-statement-rest ()
  (make-instance 'loop-statement-node
                 :statements-list (prog1 (parse-statements-list)
                                    (pop-bind-tokens (1 2)
                                      (if-unexpected-throw-unexpected (token1-type :endloop
                                                                       token2-type :semicolon))))))

(defun parse-for-statement-rest ()
  (make-instance 'for-statement-node
                 :variable (pop-bind-tokens (1 2)
                             (if-unexpected-throw-unexpected (token1-type :user-defined-identifier
                                                              token2-type :assignment)
                               (token-lexem token1)))
                 :from-expression (prog1 (infix-tokens->prefix-sexp)
                                    (pop-bind-tokens (1 1)
                                      (if-unexpected-throw-unexpected (token1-type :to))))
                 :to-expression (prog1 (infix-tokens->prefix-sexp)
                                  (pop-bind-tokens (1 2)
                                    (if-unexpected-throw-unexpected (token1-type :endfor
                                                                     token2-type :semicolon))))))
