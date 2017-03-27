(in-package :translator-parser)

;; dynamic vars

(defparameter *token-list* nil)

(defparameter *tree* nil)

(defparameter *stack* nil)

;; conditions

(define-condition unexpected-token (translator-condition)
  ((expected :reader expected :initarg :expected :initform nil)
   (actual :reader actual :initarg :actual :initform nil)))

(defmethod print-object ((c unexpected-token) stream)
  (format stream "~A~%expected: ~A~%actual: ~A~%" (message c) (expected c) (actual c)))

;; nodes

(defmacro defnode (name direct-superclasses direct-slots)
  `(defclass ,name ,direct-superclasses
     ,(mapcar (lambda (slotname)
                `(,slotname :accessor ,(symbolicate "NODE-" (write-to-string slotname))
                            :initarg ,(intern (write-to-string slotname) "KEYWORD")
                            :initform nil))
       direct-slots)))

(defnode program-node ()
  (name
   statements-list))

(defnode loop-statement-node ()
  (statements-list))

(defnode for-statement-node ()
  (variable
   from-expression ;; prefix notation list
   to-expression
   statements-list))
