;;; test-helper --- Test helper for emacs-fireplace

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar fireplace-test-path
  (f-dirname (f-this-file)))

(defvar fireplace-root-path
  (f-parent fireplace-test-path))

(defvar fireplace-sandbox-path
  (f-expand "sandbox" fireplace-test-path))

(when (f-exists? fireplace-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" fireplace-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory fireplace-sandbox-path))
     (when (f-exists? fireplace-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir fireplace-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
    (require 'cl))
(require 'undercover)
(undercover "*.el"
	    (:exclude "*-test.el")
	    (:send-report nil)
	    (:report-file "/tmp/undercover-report.json"))
(require 'fireplace)

(provide 'test-helper)
;;; test-helper.el ends here
