(in-package :translator-parser)

(defun parser (token-list)
  (let ((*token-list* token-list)
        (*tree* (make-tree))))
  (program))
