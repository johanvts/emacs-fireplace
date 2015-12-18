;;; fireplace.el --- A cozy fireplace for emacs
;;; Version: 1.0
;;; Commentary:
;; Author: Johan Sivertsen <johanvts@gmail.com>
;; URL: https://github.com/johanvts/emacs-fireplace
;; Released: December 2015

;;; Code:
;; User definable Variables
(defvar fireplace-smoke-on nil
  "Controls if smoke is drawn of not.")

(defvar fireplace-fury 0.85
  "The redraw speed of the fire. Between 0 and 1.")

(defvar fireplace-smoke-char ?\*
  "Char used for drawing smoke.")

(defvar fireplace-background-char ?\s
  "Char used for filling in the background.")

(defvar fireplace-fill-char ?\s
  "Char used for drawing smoke.")

(defvar fireplace-flame-pos '(0.5 0.2 0.8 0.36 0.64 )
  "Relative position and order for drawing flames.")


(defvar fireplace-buffer-name  "*fireplace*"
  "Default name for fireplace buffer.")

;; Program controlled variables

(defvar fp-bkgd-height "Used for fireplace height, will be set from windows size")
(defvar fp-bkgd-width "Used for fireplace width, will be set from windows size")
(defvar fp-timer "Holds the active fireplace, kill using fireplace-off")
(defvar fp-flame-width "Calculated width of flames")

;; Helper routines

(defun make-grid ()
  (erase-buffer)
  (dotimes (i fp-bkgd-height)
    (insert-char fireplace-background-char fp-bkgd-width)
    (newline)))

(defun gotoxy (x y)
  (goto-char (+ 1 x (* (- fp-bkgd-height (+ 1 y)) (+ 1 fp-bkgd-width)))))


(defun draw-flame-stripe (x y width)
  (gotoxy x y)
  (let* ((actual-width (min width (1+ (- fp-bkgd-width x))))
	 (hot-core (/ actual-width 2)))
    (delete-char actual-width)
    (insert (propertize (make-string actual-width fireplace-fill-char)
			'face `(:background ,"orange red")))
    (when (> hot-core 1)
      (gotoxy (+ x (/ hot-core 2)) y)
      (delete-char hot-core)
      (insert (propertize (make-string hot-core fireplace-fill-char)
				      'face `(:background ,"dark orange"))))))

(defun smoke (x height)
  (gotoxy (if (>(random 3) 1)
	      (+ x (random (/ fp-bkgd-width 5)))
	    (max 0 (- x (random (/ fp-bkgd-width 5)))))
	  (+ height (random (- fp-bkgd-height height))))
  (delete-char 1)
  (insert (propertize (make-string 1 fireplace-smoke-char)
		      'face `(:foreground, "slate grey"))))

(defun flame (middle h)
  (setq cursor-type nil)
  (let* ((width h)
	 (lower (truncate(* 0.2 h)))
	 (high (- h lower))
	 x
	 line)
    (dotimes (y lower)
      (setq width (+ width y))
      (setq x (- middle (/ width 2)))
      (when (< x 0)
	(setq width (+ width x))
	(setq x 0))
      (when (> (+ x width) fp-bkgd-width)
	(setq width (- fp-bkgd-width x)))
      (draw-flame-stripe x y width))
    (dotimes (y high)
      (setq line (+ lower y))
      (setq width (max 0 (- width 1 (random 3))))
      (setq x (- middle (/ width 2)))
      (when (< x 0)
	(setq width (+ width x))
	(setq x 0))
      (when (> (+ x width) fp-bkgd-width)
	(setq width (- fp-bkgd-width x)))
      (draw-flame-stripe x line width)
      (when fireplace-smoke-on (smoke x h)))))

(defun draw-fireplace (buffer-name flame-pos flame-width)
  (with-current-buffer (get-buffer-create buffer-name)
    (setq buffer-read-only nil)
    (make-grid)
    (dolist (pos flame-pos)
      (flame (round (* pos fp-bkgd-width))
	     (+
	      (round (* (+ 0.2 (min pos (- 1 pos))) flame-width))
	      (random 3))))
    (setq buffer-read-only t)))


;;Commands

(defun fireplace (arg)
  (interactive "P")
  (with-current-buffer (get-buffer-create fireplace-buffer-name)
    (setq cursor-type nil)
    (buffer-disable-undo)
    (switch-to-buffer fireplace-buffer-name)
    (setq fp-bkgd-height (round (window-height (get-buffer-window fireplace-buffer-name))))
    (setq fp-bkgd-width  (round (window-width (get-buffer-window fireplace-buffer-name))))
    (setq fp-flame-width (min fp-bkgd-height (round (/ fp-bkgd-width 2.5))))
    (make-grid)
    (fireplace-mode)
    (setq fp-timer (run-with-timer 1 (- 1 fireplace-fury)
					  'draw-fireplace fireplace-buffer-name fireplace-flame-pos fp-flame-width))))

(defun fireplace-off ()
  "Put out the fire."
  (interactive)
  (when fp-timer
    (cancel-timer fp-timer)
    (kill-buffer fireplace-buffer-name)))

(defun fireplace-down ()
  (interactive)
  "Push the fire further down"
  (setq fp-bkgd-height (+ fp-bkgd-height 1)))


(defun fireplace-up ()
  (interactive)
  "Move the fire further up"
  (setq fp-bkgd-height (max 0 (- fp-bkgd-height 1))))

(defun fireplace-toggle-smoke ()
  (interactive)
  "Toggle smoke on/off"
  (if fireplace-smoke-on
      (setq fireplace-smoke-on nil)
    (setq fireplace-smoke-on t)))

(provide 'fireplace)
(provide 'fireplace-off)
(provide 'fireplace-down)

;;Key-bindings

(define-derived-mode fireplace-mode special-mode
  "A cozy fireplace")


(define-key fireplace-mode-map (kbd "C-+") 'fireplace-down)
(define-key fireplace-mode-map (kbd "C--") 'fireplace-up)
(define-key fireplace-mode-map (kbd "C-s") 'fireplace-toggle-smoke)

;;; fireplace.el ends here
