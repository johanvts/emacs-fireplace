;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I try to configure \"\\([^\"]+\\)\"$"
      (lambda (group)
        (shut-up
         (customize-group group))))

(Then "^there is a \"\\([^\"]+\\)\" buffer$"
      (lambda (buffer-name)
        (get-buffer buffer-name)))

(When "^I wait for \\([0-9]+\\) second$"
     (lambda (time)
       (sleep-for (string-to-number time))))
