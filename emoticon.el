(require 'cl-lib)
(require 'helm)
(require 'emoticon-data)
(defvar emoticon-max-candidate 9999)
(defvar emoticon-buffer-name "*(●´ϖ`●)*")
(defvar emoticon-prompt "Search (¬､¬)")

(defun emoticon ()
  (interactive)
  (helm :sources (helm-build-sync-source "Search emoticon... (´・ω・`)"
                   :candidates (lambda () (emoticon-get-candidates helm-pattern))
                   :volatile t
                   :action (lambda (str) (insert str))
                   :candidate-number-limit emoticon-max-candidate)
        :buffer emoticon-buffer-name
        :prompt emoticon-prompt))

(defun emoticon-get-candidates (pattern)
  (cl-remove-if
   #'null
   (mapcar (lambda (row)
             (print row)
             (let ((matched (string-match pattern (car row))))
               (if matched
                   (cons (concat (car row) "   =>   " (cdr row))
                         (cdr row))
                 nil)))
           emoticon-table
           )))

(provide 'emoticon)
