(in-package :translator-lexer)

;; conditions

(define-condition wrong-character (translator-condition warning)
  ((wrong-char :reader wrong-char :initarg :wrong-char :initform nil)
   (line :reader line :initarg :line :initform nil)
   (column :reader column :initarg :column :initform nil)))

(defmethod print-object ((c wrong-character) stream)
  (format stream "~A~%character: ~A~%line: ~A~%column: ~A~%"
          (message c) (wrong-char c) (line c) (column c)))

(define-condition empty-file (translator-condition)
  ((file :reader file :initarg :file :initform nil)))

(defmethod print-object ((c empty-file) stream)
  (format stream "~A~%file: ~A~%" (message c) (file c)))

;; dynamic vars

(defparameter *token-list* nil)

(defparameter *keywords* (plist-hash-table '("PROGRAM" program
                                             "BEGIN" begin
                                             "END" end
                                             "LOOP" loop
                                             "ENDLOOP" endloop
                                             "FOR" for
                                             "ENDFOR" endfor
                                             "TO" to
                                             "DO" do
                                             ":=" assign
                                             "+" plus
                                             "-" minus) :test #'equal))

(defparameter *current-lexem* nil)

(defparameter *char* nil)

(defparameter *stream* nil)

(defparameter *state* nil)

(defparameter *line* nil)

(defparameter *column* nil)

(defparameter *lexem-start-column* nil)

;; functions

(defun read-next-char ()
  (setf *char* (read-char *stream* nil 'eof))
  (unless (eq *char* 'eof)
    (if (char= *char* #\newline)
        (progn (incf *line*) (setf *column* -1))
        (incf *column*))))

(defun push-token-to-token-list (&key type type-fn)
  "type-fn uses current lexem to determine it's type."
  (let* ((lexem (get-current-lexem-string))
         (sure-type (cond ((and type-fn (not type)) (funcall type-fn lexem))
                     ((and (not type-fn) type) type)
                     (t (error "add-current-lexem-to-identifiers/push-token-to-token-list: CHOOSE ONE OF TWO ARGUMENTS TO SUPPLY")))))
    ;;(when (eq sure-type 'user-defined-identifier) (add-to-identifiers lexem))
    (push (make-token :string lexem :type sure-type :line *line* :column *lexem-start-column*) *token-list*)))

(defun delimiter? (char)
  (or (<= (char-code char) 32)               ;; 32 - #\space
      (= (char-code char) 59)))              ;; 59 - #\semicolon

(defun write-char-to-current-lexem ()
  (format *current-lexem* "~A" *char*))

(defun get-current-lexem-string ()
  ;; 'get-output-stream-string' also clears the stream
  (get-output-stream-string *current-lexem*))

(defun what-keyword? (lexem)
  (gethash lexem *keywords*))

(defun get-keyword-type (keyword)
  (gethash keyword *keywords*))

;; read file char by char

;; (defmacro read-file-by-char ((filename char &key stream-binding-symbol) &body body)
;;   (let ((stream (if-let ((it stream-binding-symbol)) it (gensym "STREAM-")))
;;         (eof-value (gensym "EOF-"))
;;         (eof-value-with-error (gensym "EOF-ERROR")))
;;     `(with-open-file (,stream ,filename)
;;        (do ((,char (read-char ,stream nil ',eof-value-with-error)
;;                    (read-char ,stream nil ',eof-value)))
;;            ((cond ((eq ,char ',eof-value)
;;                    (if (or (eq *state* :comment)
;;                            (eq *state* :comment-end))
;;                        (error 'wrong-character :message "Unexpected end of file after the following position."
;;                                                :line *line* :column *column* :wrong-char 'eof)
;;                        t))
;;                   ((eq ,char ',eof-value-with-error) (error 'translator-common:empty-file :message "This file is empty" :file ,filename))))
;;          ,@body))))
