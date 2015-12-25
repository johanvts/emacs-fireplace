(require 'f)

(defvar fireplace-support-path
  (f-dirname load-file-name))

(defvar fireplace-features-path
  (f-parent fireplace-support-path))

(defvar fireplace-root-path
  (f-parent fireplace-features-path))

(add-to-list 'load-path fireplace-root-path)

(require 'undercover)
(undercover "*.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))
(require 'fireplace)
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
 ;; (fireplace-off) ; so far it break if no fireplace buffer
 )

(Teardown
 ;; After when everything has been run
 )
