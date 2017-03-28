;;;; small testing framework as in PCL

(in-package #:test-utils)

(defmacro deftest (test-name let-list form-tested form-expected test-fn)
  "Defines a function with name 'test-name' which s set to use
'test-fn' to compare evaluation results from 'form-tested' and 'form-expected'.
If 'test-fn' returnes nil - signals simple condition.
If 'test-fn' doesn't return nil - returnes that value."
  `(defun ,test-name ()
     (let* (,@let-list
            (result ,form-tested)
            (expected ,form-expected))
       (if-let ((it (funcall ,test-fn result expected)))
            it 
            (error "error in test ~A:~%expected:~%~S~%result:~%~S~%"
                   ',test-name expected result)))))

(defmacro deftests (lab-test-name let-list &rest test-lists-list)
  "Takes let-list like ((a 1) (b 2)) and then 
lists like (test-task1 (cons 1 (cons 'a (cons 2 (cons 'b nil)))) '(1 a 2 b) #'test-func)
and produces test functions with 'deftest' and then calls them."
  (let* ((test-func-calls)
         (global-let-list (mapcar (lambda (let-form) `(,(gensym) ,(second let-form))) let-list))
         (local-let-list (map 'list (lambda (orig-let-form gen-global-let-form)
                                      `(,(first orig-let-form) ,(first gen-global-let-form)))
                              let-list global-let-list)))
    (labels ((%expand-test-lists-list-and-collect-test-func-calls (test-lists-list n) ; n to control the number of dots in a row
               (when test-lists-list
                 (let* ((test-list (first test-lists-list))
                        (test-name (first test-list))
                        (form-tested (second test-list))
                        (form-expected (third test-list))
                        (test-fn (fourth test-list))
                        (test-full-name (intern (concatenate 'string (write-to-string lab-test-name) "-" (write-to-string test-name)))))
                   ;; while generating 'deftests' i also collect a form in which test function should be called to 'test-func-calls'.
                   (push `(format t "~A"
                                  (handler-case (,test-full-name)
                                    (error (se) (declare (ignore se)) (push ',test-full-name failed-test-func-names) "x")
                                    (:no-error (ret-val) (declare (ignore ret-val)) (push ',test-full-name passed-test-func-names) ".")))
                         test-func-calls)
                   ;; if there are 30 dots and exses in a row push format with newline to a call
                   (when (= n 30)
                     (setf n 1)
                     (push `(format t "~%") test-func-calls))
                   ;; make list of 'deftests'
                   (cons `(deftest ,test-full-name ,local-let-list ,form-tested ,form-expected ,(if test-fn test-fn '(function equalp)))
                         (%expand-test-lists-list-and-collect-test-func-calls (cdr test-lists-list) (1+ n)))))))      
      `(let* ,global-let-list
         ,@(append
            ;; defining test functions
            (%expand-test-lists-list-and-collect-test-func-calls test-lists-list 1)
            ;; defining tests function
            `((defun ,lab-test-name ()
                (let ((failed-test-func-names)
                      (passed-test-func-names))
                  (format t "--------------------------------------------------~%")
                  (format t "*** ~A ***~%" ',lab-test-name)
                  ;; calling functions test functions
                  ,@(reverse (cons '(format t "~%") test-func-calls))
                  ;; report passed tests
                  (format t "PASSED:~%~A"
                          (if passed-test-func-names
                              (format nil "~{ + ~A~%~}" (reverse passed-test-func-names))
                              (format nil " + NONE~%")))
                  ;; report failed tests
                  (format t "FAILED:~%~A~%" 
                          (if failed-test-func-names
                              (format nil "~{ - ~A~%~}" (reverse failed-test-func-names))
                              (format nil " - NONE~%")))
                  ;; return nil if failed and t if succeed
                  (null failed-test-func-names)))))))))
