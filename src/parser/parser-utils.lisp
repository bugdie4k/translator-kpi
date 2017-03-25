(in-package :translator-parser)

(defmacro defnode (name direct-superclasses direct-slots)
  `(defclass ,name ,direct-superclasses
     ,(mapcar (lambda (slotname)
                `(,slotname :accessor ,(symbolicate "NODE-" (write-to-string slotname))
                            :initarg ,(intern (write-to-string slotname) "KEYWORD")
                            :initform nil))
       direct-slots)))

(defnode program-node ()
  (name block))

;; nodes

;; (defnode position-mixin () (line column))

(defnode program-node ()
  (name
   statements-list))

(defnode loop-statement ()
  (statements-list))

(defnode for-statement ()
  (variable
   from-expression
   to-expression
   statements-list))

(defnode expression ()
  prefix-notation-list)

;; parse expression

;; dynamic vars

(defparameter *token-list* nil)

(defparameter *tree* nil)

(defparameter *stack* nil)
