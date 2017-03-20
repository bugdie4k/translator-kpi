(in-package :translator-lexer)

;; conditions

(define-condition wrong-character (translator-condition warning)
  ((wrong-char :reader wrong-char :initarg :wrong-char :initform nil)
   (line :reader line :initarg :line :initform nil)
   (column :reader column :initarg :column :initform nil)))

(defmethod print-object ((c wrong-character) stream)
  (format stream "~A~%character: ~A~%line: ~A~%column: ~A~%"
          (message c) (wrong-char c) (line c) (column c)))

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

;; (defparameter *delimiters* nil)

;; (defparameter *identifiers* (make-hash-table))

(defparameter *current-lexem* nil)

(defparameter *char* nil)

(defparameter *stream* nil)

(defparameter *state* nil)

(defparameter *line* nil)

(defparameter *column* nil)

(defparameter *lexem-start-column* nil)

;; functions

;; (defun add-current-lexem-to-identifiers ()
;;   (setf (gethash (get-current-lexem-string) *identifiers*) t))

;; (defun add-to-identifiers (string)
;;   (setf (gethash string *identifiers*) t))

(defun push-token-to-token-list (&key type type-fn)
  "type-fn uses current lexem to determine it's type."
  (let* ((lexem (get-current-lexem-string))
         (sure-type (cond ((and type-fn (not type)) (funcall type-fn lexem))
                     ((and (not type-fn) type) type)
                     (t (error "add-current-lexem-to-identifiers/push-token-to-token-list: CHOOSE ONE OF TWO ARGUMENTS TO SUPPLY")))))
    ;;(when (eq sure-type 'user-defined-identifier) (add-to-identifiers lexem))
    (push (make-token :string lexem :type sure-type :line *line* :column *lexem-start-column*) *token-list*)))

(defun delimiter? (char)
  (or (<= (char-code char) 32) ;; 32 - #\space
      (= (char-code char) 59)))  ;; 59 - #\semicolon

(defun write-char-to-current-lexem ()
  (format *current-lexem* "~A" *char*))

(defun get-current-lexem-string ()
  ;; 'get-output-stream-string' also clears the stream
  (get-output-stream-string *current-lexem*))

(defun what-keyword? (lexem)
  (gethash lexem *keywords*))

(defun get-keyword-type (keyword)
  (gethash keyword *keywords*))
