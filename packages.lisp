(defpackage #:translator-utils
  (:use #:cl
        #:alexandria))

(defpackage #:translator-common
  (:use #:cl
        #:alexandria
        #:translator-utils)
  (:export #:token
           #:make-token
           #:token-lexem
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
  (:export #:lexer))

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
        #:small-tests)
  (:export #:test-lexer
           #:test-parser
           #:test-all))

(defpackage #:translator
  (:use #:cl
        #:translator-lexer
        #:translator-parser)
  (:export #:lexer
           #:parser))
