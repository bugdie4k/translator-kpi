(in-package :translator-lexer)

(defun get-keyword (id)
  (car (find-if (lambda (entry) (eql (cdr entry) id)) *keywords*)))

(defun get-type (keyword)
  (cdr (find-if (lambda (entry) (string-equal (car entry) keyword)) *keywords*)))

(defun lexer (filename)
  (read-file-by-char (stream filename char 'eof #'eq)
    (format t "~A" char)))
