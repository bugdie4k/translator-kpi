(in-package :translator-parser)

;; dynamic vars

(defparameter *token-list* nil)

(defparameter *tree* nil)

(defparameter *stack* nil)

;; conditions

(define-condition parser-error (translator-condition) nil)

(define-condition unexpected-token (parser-error)
  ((expected-type :reader expected-type :initarg :expected-type :initform nil)
   (actual-token :reader actual-token :initarg :actual-token :initform nil)))

(defmethod print-object ((c unexpected-token) stream)
  (let ((tok (actual-token c)))
    (format stream "~A~%expected type: ~A~%~@[~A~]"
            (message c) (expected-type c)
            (when tok
              (format nil "actual type: ~A~%       lexem: ~A~%       line: ~A~%       column: ~A~%"
                      (token-type tok) (token-lexem tok) (token-line tok) (token-column tok))))))

;; nodes

(defmacro defnode (name direct-superclasses direct-slots)
  `(defclass ,name ,direct-superclasses
     ,(mapcar (lambda (slotname)
                `(,slotname :accessor ,(symbolicate "NODE-" (write-to-string slotname))
                            :initarg ,(intern (write-to-string slotname) "KEYWORD")
                            :initform nil))
       direct-slots)))

(defnode program-node ()
  (program-name
   statements-list))

(defnode loop-statement-node ()
  (statements-list))

(defnode for-statement-node ()
  (variable
   from-expression ;; prefix notation list
   to-expression
   statements-list))

(defmethod statement-node-to-list ((fn for-statement-node))
  `(:for :variable ,(node-variable fn) :from ,(node-from-expression fn) :to ,(node-to-expression fn)
         :statements-list ,(when-let ((nsl (node-statements-list fn)))
                             (mapcar (lambda (stmt)
                                       (statement-node-to-list stmt))
                                     nsl))))

(defmethod statement-node-to-list ((fn loop-statement-node))
  `(:loop :statements-list ,(when-let ((nsl (node-statements-list fn)))
                             (mapcar (lambda (stmt)
                                       (statement-node-to-list stmt))
                                     nsl))))

(defmethod program-node-to-list ((pn program-node))
  `(:program-node :name ,(node-program-name pn)
                  :statements-list ,(when-let ((nsl (node-statements-list pn)))
                                      (mapcar (lambda (stmt)
                                                (statement-node-to-list stmt))
                                              nsl))))

;; aux

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
