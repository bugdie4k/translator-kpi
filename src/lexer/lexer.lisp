(in-package :translator-lexer)

(defun ensure-char-upcase ()
  "Lowercase chars aren't alowed by the grammar, but it is not a big deal converting them to uppercase"
  (when (lower-case-p *char*)
    (warn 'wrong-character :message "There is a lowercase character." :char *char* :line *line* :column *column*)
    (setf *char* (char-upcase *char*))))

(defun determine-state ()
  "Used when current satate is :input. Determines state of the automata in which it should read current char and next chars.
Also handles one-char lexems like + or * instead of 'automata-pass'"
  (if (delimiter? *char*)
      :delimiter
      (progn (setf *lexem-start-column* *column*) ; no need to reset *lexem-start-column* when *char* is delimiter
             (cond ((alpha-char-p *char*)
                    (ensure-char-upcase)
                    :identifier)
                   ((digit-char-p *char*)
                    :number)
                   ((char= *char* #\colon)
                    :assignment-operator-colon)
                   ((char= *char* #\plus_sign)
                    :plus-sign)
                   ((char= *char* #\hyphen-minus) ; just #\-
                    :minus-sign)
                   (t (error 'wrong-character :message "This character is not allowed." :wrong-char *char* :line *line* :column *column*))))))

(defun automata-pass ()
  "One pass of a lexer automata on current state and char."
  (switch (*state*)
    (:input (setf *state* (determine-state)) (automata-pass))
    (:identifier (try-write-char-to-identifier))
    (:number (try-write-char-to-number))
    (:delimiter (setf *state* :input)) ;; no call to 'automata-pass' => go back to lexer and read another char
    (:assignment-operator-colon (write-char-to-current-lexem) (setf *state* :assignment-operator-equals))
    (:assignment-operator-equals (try-write-equals-to-assignment-operator))
    (:plus-sign (write-char-to-current-lexem) (push-token-to-token-list :type 'plus-sign) (setf *state* :input))
    (:minus-sign (write-char-to-current-lexem) (push-token-to-token-list :type 'minus-sign) (setf *state* :input))
    (t (error "WRONG STATE. *state*: ~A" *state*))))

(defun try-write-equals-to-assignment-operator ()
  (if (char= *char* #\equals_sign)
      (progn (write-char-to-current-lexem)
             (push-token-to-token-list :type 'assignment)
             (setf *state* :input)) ;; no call to 'automata-pass' here because new char didn't came yet
      (error 'wrong-character :message "Assignment operator is supposed because of colon character. Second character must be an equals sign."
                              :column *column* :line *line* :wrong-char *char*)))

(defun try-write-char-to-identifier ()
  (cond ((alpha-char-p *char*)
         (ensure-char-upcase)
         (write-char-to-current-lexem))
        ((digit-char-p *char*)
         (write-char-to-current-lexem))
        (t (push-token-to-token-list :type-fn (lambda (lexem) (or (what-keyword? lexem) 'user-defined-identifier)))
           (setf *state* :input)
           (automata-pass))))

(defun try-write-char-to-number ()
  (cond ((digit-char-p *char*)
         (write-char-to-current-lexem))
        (t (push-token-to-token-list :type 'number)
           (setf *state* :input)
           (automata-pass))))

(defun lexer (filename)
  "Returns list of tokens (token structure)"
  (let ((*state* :input) ; all dynamic varables used by lexer are bound
        (*char*) (*current-lexem* (make-string-output-stream))
        (*line* 1) (*column* 0) (*lexem-start-column* 0)
        (*token-list*)) 
    (read-file-by-char (filename *char*)
      (automata-pass)
      (if (char= *char* #\newline)
          (progn (incf *line*) (setf *column* 0))
          (incf *column*)))
    (nreverse *token-list*)))
