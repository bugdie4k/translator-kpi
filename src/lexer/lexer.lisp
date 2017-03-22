(in-package :translator-lexer)

(defun do-one-char (char type)
  (write-char-to-current-lexem char)
  (push-token-to-token-list :type type)
  (automaton-pass/input-determine))

(defun do-comment-end (char)
  (if (eq char 'eof)
      (error 'wrong-character :message "Unexpected end of file after the following position." :wrong-char char :line *line* :column *column*)
      (cond ((char-is (curry #'char= #\right_parenthesis) char) (automaton-pass/input-determine))
            ((char-is (curry #'char= #\asterisk) char) (do-comment-end (read-next-char)))
            (t (automaton-pass/input :comment)))))

(defun do-comment (char)
  (if (eq char 'eof)
      (error 'wrong-character :message "Unexpected end of file after the following position." :wrong-char char :line *line* :column *column*)
      (if (char= char #\asterisk)
          (automaton-pass/input :comment-end)
          (do-comment (read-next-char)))))

(defun do-comment-begin (char)
  (unless (eq char 'eof)
    (if (char-is (curry #'char= #\asterisk) (read-next-char))
        (automaton-pass/input :comment)
        (error 'wrong-character :message "Commentary is expected because of left parenthesis. Second character must be an asterisk: *."
                                :column *column* :line *line* :wrong-char char))))

(defun do-assignment-operator (char)
  (write-char-to-current-lexem char)
  (unless (eq char 'eof)
    (let ((char (read-next-char)))
      (if (char-is (curry #'char= #\equals_sign) char)
          (progn (write-char-to-current-lexem char)
                 (push-token-to-token-list :type 'assignment)
                 (automaton-pass/input-determine)) 
          (error 'wrong-character :message "Assignment operator is expected because of colon character. Second character must be an equals sign: =."
                                  :column *column* :line *line* :wrong-char char)))))

(defun do-skip-whitespaces ()
  (let ((char (read-next-char)))
    (unless (eq char 'eof)
      (if (whitespace? char)
          (do-skip-whitespaces)
          (automaton-pass/determine char)))))

(defun do-string-literal (char)
  (unless (eq char 'eof)
    (if (char-is (curry #'char= #\quotation_mark) char)       
        (progn (push-token-to-token-list :type 'string-literal) ; quotation marks aren't pushed to lexem
               (automaton-pass/input-determine))
        (progn (write-char-to-current-lexem char)
               (do-string-literal (read-next-char))))))

(defun do-number-literal (char)
  (unless (eq char 'eof)
    (if (or (char-is #'digit-char-p char)
            (char-is (curry #'char= #\full_stop) char))
        (progn (write-char-to-current-lexem char)
               (do-number-literal (read-next-char)))
        (progn (push-token-to-token-list :type 'number-literal)
               (automaton-pass/determine char)))))

(defun do-identifier (char)
  (if (char-is #'alphanumericp char)
      (progn (write-char-to-current-lexem (ensure-char-upcase char))
             (do-identifier (read-next-char)))
      (progn (push-token-to-token-list :type-fn (lambda (lexem) (or (what-keyword? lexem) 'user-defined-identifier)))
             (automaton-pass/determine char))))

(defun determine-state (char) 
  "Used to deterine automaton state depending on char."
  (if (eq char 'eof)
      :eof
      (cond ((whitespace? char) (setf *lexem-start-column* 0) :whitespace)
            (t (progn (setf *lexem-start-column* *column*) ; no need to reset *lexem-start-column* when *char* is delimiter
                      (cond ((alpha-char-p char) :identifier)
                            ((digit-char-p char) :number-literal)
                            ((char= char #\quotation_mark) :string-literal)
                            ((char= char #\colon) :assignment-operator)
                            ((char= char #\left_parenthesis) :comment-begin)
                            ((char= char #\plus_sign) :plus-sign)
                            ((char= char #\hyphen-minus) :minus-sign) ; just #\-
                            ((char= char #\semicolon) :semicolon)
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
    (:assignment-operator (do-assignment-operator char))
    (:comment-begin (do-comment-begin char))
    (:comment (do-comment char))
    (:comment-end (do-comment-end char))
    (:plus-sign (do-one-char char 'plus-sign))
    (:minus-sign (do-one-char char 'minus-sign))
    (:semicolon (do-one-char char 'semicolon))
    (:eof)
    (t (error "WRONG STATE: ~A" state))))

(defun lexer (filename)
  "Returns list of tokens (token structure)"
  (let ((*current-lexem* (make-string-output-stream))  ; all dynamic varables used by lexer are bound
        (*line* 1) (*column* -1) (*lexem-start-column* 0)
        (*token-list*))
    (with-open-file (*stream* filename)
      (let ((char (read-next-char)))
        (if (eq char 'eof)
            (error 'empty-file :message "This file is empty." :file filename)
            (automaton-pass/determine char))))
    (nreverse *token-list*)))

