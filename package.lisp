(defpackage #:translator-utils
  (:use #:cl
        #:alexandria)
  (:export #:read-file-by-char))

(defpackage #:translator-common
  (:use #:cl
        #:alexandria
        #:translator-utils)
  (:export #:token
           #:make-token
           #:token-string
           #:token-type
           #:token-line
           #:token-column
           #:equal-token
           #:*token-list*
           ;; 
           #:translator-condition
           #:message
           ;;
           #:empty-file
           #:file))

(defpackage #:translator-lexer
  (:use #:cl
        #:alexandria
        #:translator-utils
        #:translator-common)
  (:export #:lexer
           ;; 
           #:wrong-character
           #:wrong-char
           #:line
           #:column))

(defpackage #:translator-parser
  (:use #:cl
        #:alexandria
        #:translator-utils
        #:translator-common)
  (:export #:parser))

(defpackage #:test-utils
  (:use #:cl
        #:alexandria
        #:translator-common)
  (:export #:deftest
           #:deftests))

(defpackage #:translator-tests
  (:use #:cl
        #:alexandria
        #:translator-common
        #:translator-lexer
        #:translator-parser
        #:test-utils)
  (:export #:test-lexer))

(defpackage #:translator
  (:use #:cl
        #:translator-lexer
        #:translator-parser)
  (:export #:lexer
           #:parser))
