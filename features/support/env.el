(require 'f)

(defvar emacs-fireplace-support-path
  (f-dirname load-file-name))

(defvar emacs-fireplace-features-path
  (f-parent emacs-fireplace-support-path))

(defvar emacs-fireplace-root-path
  (f-parent emacs-fireplace-features-path))

(add-to-list 'load-path emacs-fireplace-root-path)

(require 'undercover)
(undercover "*.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))
(require 'emacs-fireplace)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
