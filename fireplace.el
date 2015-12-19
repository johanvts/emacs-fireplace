;;; fireplace.el --- A cozy fireplace for emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Johan Sivertsen
;;; Version: 0.1
;;; Author: Johan Sivertsen <johanvts@gmail.com>
;;; URL: https://github.com/johanvts/emacs-fireplace
;;; Released: December 2015

;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Puts your emacs on fire

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

(defvar fireplace--bkgd-height "Used for fireplace height, will be set from windows size")
(defvar fireplace--bkgd-width "Used for fireplace width, will be set from windows size")
(defvar fireplace--timer "Holds the active fireplace, kill using fireplace-off")
(defvar fireplace--flame-width "Calculated width of flames")

;; Helper routines

(defun fireplace--make-grid ()
  (erase-buffer)
  (dotimes (i fireplace--bkgd-height)
    (insert-char fireplace-background-char fireplace--bkgd-width)
    (newline)))

(defun fireplace--gotoxy(x y)
  (goto-char (+ 1 x (* (- fireplace--bkgd-height (+ 1 y)) (+ 1 fireplace--bkgd-width)))))


(defun draw-flame-stripe (x y width)
  (fireplace--gotoxy x y)
  (let* ((actual-width (min width (1+ (- fireplace--bkgd-width x))))
	 (hot-core (/ actual-width 2)))
    (delete-char actual-width)
    (insert (propertize (make-string actual-width fireplace-fill-char)
			'face `(:background ,"dark orange")))
    (when (> hot-core 1)
      (fireplace--gotoxy (+ x (/ hot-core 2)) y)
      (delete-char hot-core)
      (insert (propertize (make-string hot-core fireplace-fill-char)
				      'face `(:background ,"orange red"))))))

(defun fireplace--smoke (x height)
  (fireplace--gotoxy (if (>(random 3) 1)
        (+ x (random (/ fireplace--bkgd-width 5)))
      (max 0 (- x (random (/ fireplace--bkgd-width 5)))))
    (+ height (random (- fireplace--bkgd-height height))))
  (delete-char 1)
  (insert (propertize (make-string 1 fireplace-smoke-char)
		      'face `(:foreground, "slate grey"))))

(defun fireplace--flame (middle h)
  (setq cursor-type nil)
  (let* ((width h)
	 (lower (truncate(* 0.2 h)))
	 (high (- h lower))
	 x
	 line)
    (dotimes (y lower)
      (setq width (+ width y)
            x (- middle (/ width 2)))
      (when (< x 0)
        (setq width (+ width x)
              x 0))
      (when (> (+ x width) fireplace--bkgd-width)
        (setq width (- fireplace--bkgd-width x)))
      (draw-flame-stripe x y width))
    (dotimes (y high)
      (setq line (+ lower y))
      (setq width (max 0 (- width 1 (random 3))))
      (setq x (- middle (/ width 2)))
      (when (< x 0)
        (setq width (+ width x)
              x 0))
      (when (> (+ x width) fireplace--bkgd-width)
        (setq width (- fireplace--bkgd-width x)))
      (draw-flame-stripe x line width)
      (when fireplace-smoke-on (fireplace--smoke x h)))))

(defun draw-fireplace (buffer-name flame-pos flame-width)
  (with-current-buffer (get-buffer-create buffer-name)
    (setq buffer-read-only nil)
    (fireplace--make-grid)
    (dolist (pos flame-pos)
      (fireplace--flame (round (* pos fireplace--bkgd-width))
	     (+
	      (round (* (+ 0.2 (min pos (- 1 pos))) flame-width))
	      (random 3))))
    (setq buffer-read-only t)))


;; Commands
;;;###autoload
(defun fireplace (arg)
  "Turn on the fire like it's winter."
  (interactive "P")
  (with-current-buffer (get-buffer-create fireplace-buffer-name)
    (setq cursor-type nil)
    (buffer-disable-undo)
    (switch-to-buffer fireplace-buffer-name)
    (setq fireplace--bkgd-height (round (window-height (get-buffer-window fireplace-buffer-name)))
          fireplace--bkgd-width  (round (window-width (get-buffer-window fireplace-buffer-name)))
          fireplace--flame-width (min fireplace--bkgd-height (round (/ fireplace--bkgd-width 2.5))))
    (fireplace--make-grid)
    (fireplace-mode)
    (setq fireplace--timer (run-with-timer 1 (- 1 fireplace-fury)
            'draw-fireplace fireplace-buffer-name fireplace-flame-pos fireplace--flame-width))))

(defun fireplace-off ()
  "Put out the fire."
  (interactive)
  (when fireplace--timer
    (cancel-timer fireplace--timer)
    (kill-buffer fireplace-buffer-name)))

(defun fireplace-down ()
  (interactive)
  "Push the fire further down"
  (setq fireplace--bkgd-height (+ fireplace--bkgd-height 1)))


(defun fireplace-up ()
  (interactive)
  "Move the fire further up"
  (setq fireplace--bkgd-height (max 0 (- fireplace--bkgd-height 1))))

(defun fireplace-toggle-smoke ()
  (interactive)
  "Toggle smoke on/off"
  (if fireplace-smoke-on
      (setq fireplace-smoke-on nil)
    (setq fireplace-smoke-on t)))

;;Key-bindings

(define-derived-mode fireplace-mode special-mode  "A cozy fireplace")

(define-key fireplace-mode-map (kbd "C-+") 'fireplace-down)
(define-key fireplace-mode-map (kbd "C--") 'fireplace-up)
(define-key fireplace-mode-map (kbd "C-*") 'fireplace-toggle-smoke)

(provide 'fireplace)
;;; fireplace.el ends here
