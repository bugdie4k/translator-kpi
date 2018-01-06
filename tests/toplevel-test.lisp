(in-package #:translator-tests)

(defvar *test-dir*
  (let ((toplevel-dir (asdf:system-source-directory :translator)))    
    (make-pathname
     :directory (append (pathname-directory toplevel-dir)
                        '("tests" "testfiles")))))

(defun get-full-test-pathname (testname)  
  (merge-pathnames *test-dir* (pathname testname)))

(defun test-all ()
  (test-lexer)
  (test-parser))
