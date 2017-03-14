(in-package :translator-utils)

(defmacro read-file-by-char ((stream filename char eof-value eof-value-test) &body body)
  `(with-open-file (,stream ,filename)
     (do ((,char (read-char stream nil ,eof-value)
                 (read-char stream nil ,eof-value)))
         ((funcall ,eof-value-test ,char ,eof-value))
       ,@body)))
