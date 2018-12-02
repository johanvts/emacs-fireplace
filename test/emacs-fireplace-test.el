;;; Test for `emacs-fireplace'

;;; Commentary:
;; These are the tests for `emacs-fireplace'

;; Run with shell command:

;; emacs -Q --batch --directory ./ --directory ./test --load test/emacs-fireplace-test.el --funcall ert-run-tests-batch-and-exit

;;; Code:

(require 'fireplace)

(ert-deftest test-fire-raging-out-of-control ()
  :expected-result :failed
  (fireplace)
  (let ((buf (get-buffer "*fireplace*")))
    (should buf)
    (kill-buffer buf))
  (sit-for 2)
  (let ((buf (get-buffer "*fireplace*")))
    (should-not buf)))
