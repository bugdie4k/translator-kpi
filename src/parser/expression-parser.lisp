(in-package :translator-parser)


(define-condition mallformed-expression (parser-error)
  ((line :reader line :initarg :line :initform nil)
   (column :reader column :initarg :column :initform nil)))

(defmethod print-object ((c mallformed-expression) stream)
  (format stream "~A~%line: ~A~%column: ~A~%" (message c) (line c) (column c)))

(defun to-string (token)
  (case (token-type token)
    (:plus "+")
    (:minus "-")
    (:multiplication "*")
    (:division "/")
    (:power "^^")
    (:number-literal (write-to-string (token-lexem token)))
    (:user-defined-identifier (token-lexem token))
    (:left-parenthesis "(")
    (:right-parenthesis ")")
    (t :end-of-expr)))

(defun infix-tokens->prefix-sexp ()
  (named-readtables:in-readtable cmu-infix:syntax)
  (let* ((1st-token (first *token-list*))
         (start-line (token-line 1st-token))
         (start-column (token-column 1st-token)))
    (handler-case (cmu-infix:string->prefix
                   (with-output-to-string (expr-stream)
                     (loop for token in *token-list* do
                       (let ((str (to-string token)))
                         (if (eq str :end-of-expr)
                             (return)
                             (progn (format expr-stream str)
                                    (pop *token-list*)))))))
      (error (e) (declare (ignore e)) (error 'mallformed-expression :message "There is a mallformed expression." :line start-line :column start-column)))))
