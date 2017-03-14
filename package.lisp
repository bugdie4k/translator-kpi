(defpackage #:translator-utils
  (:use #:cl)
  (:export #:read-file-by-char))

(defpackage #:translator-common
  (:use #:cl #:alexandria #:translator-utils)
  (:export #:*keywords*
           #:*identifiers*))

(defpackage #:translator-lexer
  (:use #:cl #:alexandria #:translator-utils #:translator-common)
  (:export #:lexer))

(defpackage #:translator-parser
  (:use #:cl #:alexandria #:translator-utils #:translator-common)
  (:export #:parser))

(defpackage #:translator-tests
  (:use #:cl #:translator-lexer #:translator-parser))

(defpackage #:translator
  (:use #:cl #:translator-lexer #:translator-parser)
  (:export #:lexer #:parser))
