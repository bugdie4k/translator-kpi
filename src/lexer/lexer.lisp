(in-package :translator-lexer)

(defun ensure-char-upcase ()
  "Lowercase chars aren't alowed by the grammar, but it is not a big deal converting them to uppercase"
  (when (lower-case-p *char*)
    (warn 'wrong-character :message "There is a lowercase character." :char *char* :line *line* :column *column*)
    (setf *char* (char-upcase *char*))))

;; (defun determine-state (char)
;;   "Used to deterine automaton state depending on char."
;;   ())

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
                    :assignment-operator)
                   ((char= *char* #\plus_sign)
                    :plus-sign)
                   ((char= *char* #\hyphen-minus) ; just #\-
                    :minus-sign)
                   ((char= *char* #\left_parenthesis)
                    :comment-begin)
                   (t (error 'wrong-character :message "This character is not allowed." :wrong-char *char* :line *line* :column *column*))))))

(defun automata-pass () ;; TODO: maybe remove global *char* and *state*
  "One pass of a lexer automata on current state and char."
   (format t "~A::~A (~A,~A)~%" *state* *char* *line* *column*)
  (unless (eq *char* 'eof)
    (switch (*state*)
      (:input (read-next-char) (unless (eq *char* 'eof) (setf *state* (determine-state)) (automata-pass)))
      (:identifier (try-write-char-to-identifier))
      (:number (try-write-char-to-number))
      (:delimiter (automata-pass-input)) ;; no call to 'automata-pass' => go back to lexer and read another char
      (:assignment-operator (try-get-assignment-operator))
      ;; (:assignment-operator-colon (write-char-to-current-lexem) (setf *state* :assignment-operator-equals))
      ;; (:assignment-operator-equals (try-write-equals-to-assignment-operator))
      (:plus-sign (write-char-to-current-lexem) (push-token-to-token-list :type 'plus-sign) (automata-pass-input))
      (:minus-sign (write-char-to-current-lexem) (push-token-to-token-list :type 'minus-sign) (automata-pass-input))
      (:comment-begin (try-comment-begin))
      (:comment (read-next-char) (when (char= *char* #\asterisk) (setf *state* :comment-end)) (automata-pass))
      (:comment-end (try-end-commentary))
      ;; (:comment-begin-lpar (setf *state* :comment-begin-asterisk))
      ;; (:comment-begin-asterisk (try-get-asterisk-for-commentary))
      ;; (:comment (when (char= *char* #\asterisk) (setf *state* :comment-end)))
      ;; (:comment-end (try-end-commentary))    
      (t (error "WRONG STATE. *state*: ~A" *state*)))))

(defun automata-pass-input ()
  (setf *state* :input)
  (automata-pass))

(defun try-comment-begin ()
  (read-next-char)
  (if (char= *char* #\asterisk)
      (progn (setf *state* :comment) (automata-pass))
      (error 'wrong-character :message "Commentary is expected because of left parenthesis. Second character must be an asterisk: *."
                            :column *column* :line *line* :wrong-char *char*)))

(defun try-get-assignment-operator ()
  (write-char-to-current-lexem)
  (read-next-char)
  (if (char= *char* #\equals_sign)
      (progn (write-char-to-current-lexem)
             (push-token-to-token-list :type 'assignment)
             (automata-pass-input)) 
      (error 'wrong-character :message "Assignment operator is expected because of colon character. Second character must be an equals sign: =."
                              :column *column* :line *line* :wrong-char *char*)))

(defun try-end-commentary ()
  (read-next-char)
  (cond ((char= *char* #\right_parenthesis) (automata-pass-input))
        ((char= *char* #\asterisk) (automata-pass))
        (t (setf *state* :comment) (automata-pass))))

(defun try-write-equals-to-assignment-operator ()
  (if (char= *char* #\equals_sign)
      (progn (write-char-to-current-lexem)
             (push-token-to-token-list :type 'assignment)
             (setf *state* :input)) ;; no call to 'automata-pass' here because new char didn't came yet
      (error 'wrong-character :message "Assignment operator is expected because of the colon character. Second character must be an equals sign: =."
                              :column *column* :line *line* :wrong-char *char*)))

(defun try-write-char-to-identifier ()
  (cond ((or (alpha-char-p *char*)
             (digit-char-p *char*))
         (ensure-char-upcase)
         (write-char-to-current-lexem)
         (read-next-char))
        ;; ((digit-char-p *char*)
        ;;  (write-char-to-current-lexem))
        (t (push-token-to-token-list :type-fn (lambda (lexem) (or (what-keyword? lexem) 'user-defined-identifier))) (setf *state* :input)))
  (automata-pass))

(defun try-write-char-to-number ()
  (cond ((digit-char-p *char*)
         (write-char-to-current-lexem)
         (read-next-char))
        (t (push-token-to-token-list :type 'number) (setf *state* :input)))
  (automata-pass))

(defun lexer (filename)
  "Returns list of tokens (token structure)"
  (let ((*state* :input) ; all dynamic varables used by lexer are bound
        (*char*) (*current-lexem* (make-string-output-stream))
        (*line* 1) (*column* -1) (*lexem-start-column* 0)
        (*token-list*))
    (with-open-file (*stream* filename)
      ;; (read-file-by-char (filename *char*))
      (automata-pass))
    (nreverse *token-list*)))

