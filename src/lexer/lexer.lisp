(in-package :translator-lexer)

(defun do-one-char (char type)
  (write-char-to-current-lexem char)
  (push-token-to-token-list :type type)
  (let ((char2 (read-next-char)))
    (if (eq type :full-stop)
        (automaton-pass/determine char2)
        (with-eof-check (char2)
          (automaton-pass/determine char2)))))

(defun do-comment-end (char)
  (with-eof-check (char)
    (cond ((char= #\right_parenthesis char) (automaton-pass/input-determine))
          ((char= #\asterisk char) (do-comment-end (read-next-char)))
          (t (automaton-pass/input :comment)))))

(defun do-comment (char)
  (with-eof-check (char)
    (if (char= char #\asterisk)
        (automaton-pass/input :comment-end)
        (do-comment (read-next-char)))))

(defun do-left-parensesis (char)
  (with-eof-check (char)
    (let ((char2 (read-next-char)))
      (with-eof-check (char2)
        (if (char= char2 #\asterisk)
            (automaton-pass/input :comment)
            (progn (write-char-to-current-lexem char)
                   (push-token-to-token-list :type :left-parenthesis)
                   (cond ((char= char2 #\hyphen-minus)
                          (automaton-pass char2 :minus))
                         ((char= char2 #\plus_sign)
                          (automaton-pass char2 :plus))
                         ((char= char2 #\left_parenthesis)
                          (automaton-pass char2 :left-parenthesis))
                         ((digit-char-p char2)
                          (automaton-pass char2 :number-literal)))))))))

(defun do-assignment-operator (char)
  (write-char-to-current-lexem char)
  (with-eof-check (char)
   (let ((char (read-next-char)))
     (if (char= #\equals_sign char)
         (progn (write-char-to-current-lexem char)
                (push-token-to-token-list :type :assignment)
                (automaton-pass/input-determine)) 
         (error 'wrong-character :message "Assignment operator is expected because of colon character. Second character must be an equals sign: =."
                                 :column *column* :line *line* :wrong-char char)))))

(defun do-skip-whitespaces ()
  (let ((char (read-next-char)))
    (with-eof-check (char)
      (if (whitespace? char)
          (do-skip-whitespaces)
          (automaton-pass/determine char)))))

(defun do-string-literal (char)
  (with-eof-check (char)
    (if (char= #\quotation_mark char)       
        (progn (push-token-to-token-list :type :string-literal) ; quotation marks aren't pushed to lexem
               (automaton-pass/input-determine))
        (progn (write-char-to-current-lexem char)
               (do-string-literal (read-next-char))))))

(defun do-number-literal (char)
  (with-eof-check (char)
    (if (or (digit-char-p char)
            (char= #\full_stop char))
        (progn (write-char-to-current-lexem char)
               (do-number-literal (read-next-char)))
        (progn (push-token-to-token-list :type :number-literal)
               (automaton-pass/determine char)))))

(defun do-identifier (char)
  (with-eof-check (char)
    (if (alphanumericp char)
        (progn (write-char-to-current-lexem (ensure-char-upcase char))
               (do-identifier (read-next-char)))
        (progn (push-token-to-token-list :type-fn (lambda (lexem) (or (what-keyword? lexem) :user-defined-identifier)))
               (automaton-pass/determine char)))))

(defun determine-state (char) 
  "Used to deterine automaton state depending on char."
  (if (eq char 'eof)
      :eof
      (cond ((whitespace? char) (setf *lexem-start-column* 0) :whitespace)
            (t (progn (setf *lexem-start-column* *column*) ; no need to reset *lexem-start-column* when *char* is delimiter
                      (cond ((alpha-char-p char) :identifier)
                            ((digit-char-p char) :number-literal)
                            ((char= char #\quotation_mark) :string-literal)
                            ((char= char #\colon) :assignment)
                            ((char= char #\left_parenthesis) :left-parenthesis)
                            ((char= char #\right_parenthesis) :right-parenthesis)
                            ((char= char #\plus_sign) :plus)
                            ((char= char #\hyphen-minus) :minus) ; just #\-
                            ((char= char #\asterisk) :multiplication)
                            ((char= char #\solidus) :division) ; slash
                            ((char= char #\circumflex_accent) :power)
                            ((char= char #\semicolon) :semicolon)
                            ((char= char #\full_stop) :full-stop)
                            ;; rest is error
                            (t (error 'wrong-character :message "This character is not allowed." :wrong-char char :line *line* :column *column*))))))))

(defun automaton-pass/determine (char)
  "Convinience wrapper around 'automaton-pass'"
  (automaton-pass char (determine-state char)))

(defun automaton-pass/input (state)
  "Convinience wrapper around 'automaton-pass'"
  (automaton-pass (read-next-char) state))

(defun automaton-pass/input-determine ()
  "Convinience wrapper around 'automaton-pass'"
  (automaton-pass/determine (read-next-char)))

(defun automaton-pass (char state) 
  "One pass of a lexer automaton."
  (case state
    (:identifier (do-identifier char))
    (:number-literal (do-number-literal char))
    (:string-literal (do-string-literal char))
    (:whitespace (do-skip-whitespaces))
    (:assignment (do-assignment-operator char))
    (:left-parenthesis (do-left-parensesis char))
    (:right-parenthesis (do-one-char char :right-parenthesis))
    (:comment (do-comment char))
    (:comment-end (do-comment-end char))
    (:plus (do-one-char char :plus))
    (:minus (do-one-char char :minus))
    (:multiplication (do-one-char char :multiplication))
    (:division (do-one-char char :division))
    (:power (do-one-char char :power))
    (:semicolon (do-one-char char :semicolon))
    (:full-stop (do-one-char char :full-stop))
    (:eof)
    (t (error "WRONG STATE: ~A" state))))

(defun lexer (filename-or-program-string &key is-program-string)
  "Returns list of tokens (see token structure at src/common/translator-common.lisp)"
  (let ((*current-lexem* (make-string-output-stream))  ; all dynamic varables used by lexer are bound
        (*line* 1) (*column* -1) (*lexem-start-column* 0)
        (*token-list*))
    (if is-program-string ; ugly, hehe
        (with-input-from-string (*stream* filename-or-program-string)
          (let ((char (read-next-char)))
            (if (eq char 'eof)
                (error "The string passed to lexer is empty.")
                (automaton-pass/determine char))))
        (with-open-file (*stream* filename-or-program-string)
          (let ((char (read-next-char)))
            (if (eq char 'eof)
                (error 'empty-file :message "This file is empty." :file filename-or-program-string)
                (automaton-pass/determine char)))))
    (nreverse *token-list*)))

