(in-package :translator-common)

(defparameter *keywords* (plist-hash-table '("program" 'program
                            "begin" 'begin
                            "end" 'end
                            "loop" 'loop
                            "endloop" 'endloop
                            "for" 'for
                            "endfor" 'endfor
                            "to" 'to
                            "do" 'do
                            ":=" 'assign
                            "+" 'plus
                            "-" 'minus)))

(defparameter *identifiers* nil)
