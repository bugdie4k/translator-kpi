(in-package :translator-common)

;; token definitions

(defstruct token
  type
  lexem
  line
  column)

(defun token-equal (token1 token2)
  (and (string= (token-lexem token1) (token-lexem token2))
       (eq (token-type token1) (token-type token2))
       (= (token-line token1) (token-line token2))
       (= (token-column token1) (token-column token2))))

(defparameter *token-list* nil)

;; parent condition

(define-condition translator-condition (condition)
  ((message :reader message :initarg :message :initform nil)))

(defmethod print-object ((c translator-condition) stream)
  (format stream "~A~%" (message c)))

