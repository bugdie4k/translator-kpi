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

(defparameter *keywords* (plist-hash-table '("PROGRAM" :program
                                             "BEGIN" :begin
                                             "END" :end
                                             "LOOP" :loop
                                             "ENDLOOP" :endloop
                                             "FOR" :for
                                             "ENDFOR" :endfor
                                             "TO" :to
                                             "DO" :do
                                             ":=" :assign
                                             "+" :plus
                                             "-" :minus) :test #'equal))

(defparameter *current-lexem* nil)

(defparameter *stream* nil)

(defparameter *line* nil)

(defparameter *column* nil)

(defparameter *lexem-start-column* nil)

;; convinience functions and macros

(defmacro with-eof-check ((char-var) &body body)
  `(if (eq ,char-var 'eof)
       (error 'wrong-character :message "Unexpected end of file at the following position." :wrong-char ,char-var :line *line* :column *column*)
       (progn ,@body)))

(defun char-is (fn char)
  (unless (eq char 'eof)
    (funcall fn char)))

(defun ensure-char-upcase (char)
  "Lowercase chars aren't alowed by the grammar, but it is not a big deal converting them to uppercase"
  (if (lower-case-p char)
      (progn (warn 'wrong-character :message "There is a lowercase character." :wrong-char char :line *line* :column *column*)     
             (char-upcase char))
      char))

(defun read-next-char ()
  "Read char from stream bound to dynamic variable *stream*"
  (let ((char (read-char *stream* nil 'eof)))
    (if (or (eq char 'eof)
            (not (char= char #\newline)))
        (incf *column*)
        (progn (incf *line*) (setf *column* -1)))
    char))

(defun string2char (str)
  (if (> (length str) 1)
      (first (coerce 'list str))
      (error "Argument is not a one char string: ~A" str)))

(defun push-token-to-token-list (&key type type-fn)
  "type-fn uses current lexem to determine it's type."
  (let* ((lexem (get-current-lexem-string))
         (sure-type (cond ((and type-fn (not type)) (funcall type-fn lexem))
                          ((and (not type-fn) type) type)
                          (t (error "add-current-lexem-to-identifiers/push-token-to-token-list: CHOOSE ONE OF TWO ARGUMENTS TO SUPPLY"))))
         (sure-lexem (case sure-type
                       ;;(:keyword (intern lexem :keyword))
                       (:number-literal (parse-number:parse-number lexem))
                       ;;(:string-literal lexem)
                       (t lexem))))
    (push (make-token :lexem sure-lexem :type sure-type :line *line* :column *lexem-start-column*) *token-list*)))

(defun whitespace? (char)
  (<= (char-code char) 32))              ;; 32 - #\space

(defun delimier? (char)
  (= (char-code char) 59))

(defun write-char-to-current-lexem (char)
  (format *current-lexem* "~A" char))

(defun get-current-lexem-string ()
  ;; 'get-output-stream-string' also clears the stream
  (get-output-stream-string *current-lexem*))

(defun what-keyword? (lexem)
  (gethash lexem *keywords*))

(defun get-keyword-type (keyword)
  (gethash keyword *keywords*))
