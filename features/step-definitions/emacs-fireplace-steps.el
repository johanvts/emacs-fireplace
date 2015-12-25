;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Then "^there is a \"\\([^\"]+\\)\" buffer$"
      (lambda (buffer-name)
        (get-buffer buffer-name)))
