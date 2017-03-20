(in-package :translator-common)

;; token definitions

(defstruct token
  string
  type
  line
  column)

(defun token-equal (token1 token2)
  (and (string= (token-string token1) (token-string token2))
       (eq (token-type token1) (token-type token2))
       (= (token-line token1) (token-line token2))
       (= (token-column token1) (token-column token2))))

(defparameter *token-list* nil)

;; parent condition

(define-condition translator-condition (condition)
  ((message :reader message :initarg :message :initform nil)))

(defmethod print-object ((c translator-condition) stream)
  (format stream "~A~%" (message c)))

(define-condition empty-file (translator-condition)
  ((file :reader file :initarg :file :initform nil)))

(defmethod print-object ((c empty-file) stream)
  (format stream "~A~%file: ~A~%" (message c) (file c)))
