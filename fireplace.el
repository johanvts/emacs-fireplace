;;; fireplace.el --- A cozy fireplace for emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Johan Sivertsen
;;; Version: 1.1.1
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

;; Puts your Emacs on fire

;;; Code:

(defgroup fireplace nil
  "Config for `fireplace' ."
  :group 'applications)


;; User definable Variables
(defcustom fireplace-smoke-on nil
  "Controls if smoke is drawn of not."
  :type 'string :group 'fireplace)

(defcustom fireplace-fury 0.85
  "The redraw speed of the fire. Between 0 and 1."
  :type 'float :group 'fireplace)

(defcustom fireplace-smoke-char ?\*
  "Char used for drawing smoke."
  :type 'character :group 'fireplace)

(defcustom fireplace-background-char ?\s
  "Char used for filling in the background."
  :type 'character :group 'fireplace)

(defcustom fireplace-fill-char ?\s
  "Char used for drawing smoke."
  :type 'character :group 'fireplace)

(defcustom fireplace-flame-pos '(0.5 0.2 0.8 0.36 0.64 )
  "Relative position and order for drawing flames."
  :type '(list float) :group 'fireplace)


(defcustom fireplace-buffer-name  "*fireplace*"
  "Default name for fireplace buffer."
  :type 'string :group 'fireplace)

;;; Faces
(defgroup fireplace-faces nil
  "Faces for `fireplace'."
  :group 'fireplace)

(defface fireplace-outter-flame-face
  '((t (:background "orange red")))
  "Color of the core of the flame."
  :group 'fireplace-faces)

(defface fireplace-inner-flame-face
  '((t (:background "dark orange")))
  "Color of the core of the flame."
  :group 'fireplace-faces)
(defface fireplace-smoke-face
  '((t (:foreground "slate grey")))
  "Color of the smoke."
  :group 'fireplace-faces)

;;; Program controlled variables
(defvar fireplace--bkgd-height "Used for fireplace height, will be set from windows size")
(defvar fireplace--bkgd-width "Used for fireplace width, will be set from windows size")
(defvar fireplace--timer "Holds the active fireplace, kill using fireplace-off")
(defvar fireplace--flame-width "Calculated width of flames")
(defvar fireplace--flame-pos "Flame position")
(defvar fireplace--flame-width "Flame width")

;;; Helper routines

(defun fireplace--make-grid ()
  "Redraw backgound of buffer."
  (erase-buffer)
  (dotimes (i fireplace--bkgd-height)
    (insert-char fireplace-background-char fireplace--bkgd-width)
    (newline)))

(defun fireplace--gotoxy(x y)
  "Move pointer to position X Y."
  (goto-char (+ 1 x (* (- fireplace--bkgd-height (+ 1 y))
                       (+ 1 fireplace--bkgd-width)))))


(defun draw-flame-stripe (x y width)
  "Draw flame stripe."
  (fireplace--gotoxy x y)
  (let* ((actual-width (min width (1+ (- fireplace--bkgd-width x))))
         (hot-core (/ actual-width 2)))
    (delete-char actual-width)
    (insert (propertize (make-string actual-width fireplace-fill-char)
      'face 'fireplace-outter-flame-face))
    (when (> hot-core 1)
      (fireplace--gotoxy (+ x (/ hot-core 2)) y)
      (delete-char hot-core)
      (insert (propertize (make-string hot-core fireplace-fill-char)
              'face 'fireplace-inner-flame-face)))))

(defun fireplace--smoke (x height)
  "Draw one random smoke."
  (fireplace--gotoxy
    (if (>(random 3) 1)
        (+ x (random (/ fireplace--bkgd-width 5)))
        (max 0 (- x (random (/ fireplace--bkgd-width 5)))))
    (+ height (random (- fireplace--bkgd-height height))))
  (delete-char 1)
  (insert (propertize (make-string 1 fireplace-smoke-char)
          'face 'fireplace-smoke-face)))

(defun fireplace--flame (middle h)
  "Draw a flame."
  (setq cursor-type nil)
  (let* ((width h)
         (lower (truncate (* 0.2 h)))
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
      (setq line (+ lower y)
            width (max 0 (- width 1 (random 3)))
            x (- middle (/ width 2)))
      (when (< x 0)
        (setq width (+ width x)
              x 0))
      (when (> (+ x width) fireplace--bkgd-width)
        (setq width (- fireplace--bkgd-width x)))
      (draw-flame-stripe x line width)
      (when fireplace-smoke-on (fireplace--smoke x h)))))

(defun fireplace-draw (buffer-name)
  "Draw the whole fireplace in BUFFER-NAME from FLAME-POS with FLAME-WIDTH."
  (with-current-buffer (get-buffer-create buffer-name)
    (setq buffer-read-only nil)
    (fireplace--make-grid)
    (dolist (pos fireplace--flame-pos)
      (fireplace--flame (round (* pos fireplace--bkgd-width))
       (+ (round (* (+ 0.2 (min pos (- 1 pos))) fireplace--flame-width))
          (random 3))))
    (setq buffer-read-only t)))

(defun fireplace--disable-minor-modes ()
  "Disable minor modes that might affect rendering."
  (switch-to-buffer fireplace-buffer-name)
  (setq truncate-lines t
        cursor-type nil
        show-trailing-whitespace nil
        show-leading-whitespace nil
        indicate-empty-lines nil)
  (transient-mark-mode nil)
  (buffer-disable-undo))

(defun fireplace--update-locals-vars (&optional frame)
  "Update `fireplace' local variables."
  (setq fireplace--bkgd-height (- (floor (window-height (get-buffer-window fireplace-buffer-name))) 1)
        fireplace--bkgd-width  (- (round (window-width (get-buffer-window fireplace-buffer-name))) 1)
        fireplace--flame-width (min fireplace--bkgd-height (round (/ fireplace--bkgd-width 2.5)))
        fireplace--flame-pos fireplace-flame-pos))

;; Commands
;;;###autoload
(defun fireplace (arg)
  "Light the fire."
  (interactive "P")
  (with-current-buffer (get-buffer-create fireplace-buffer-name)
    (fireplace--update-locals-vars)
    (fireplace--make-grid)
    (fireplace-mode)
    (add-hook 'window-size-change-functions 'fireplace--update-locals-vars)
    (fireplace--disable-minor-modes)
    (setq fireplace--timer
          (run-with-timer 1 (- 1 fireplace-fury)
                          'fireplace-draw fireplace-buffer-name))))

(defun fireplace-off ()
  "Put out the fire."
  (interactive)
  (remove-hook 'window-size-change-functions 'fireplace--update-locals-vars)
  (when fireplace--timer
    (cancel-timer fireplace--timer)
    (kill-buffer fireplace-buffer-name)))

(defun fireplace-down ()
  "Push the fire further down"
  (interactive)
  (setq fireplace--bkgd-height (+ fireplace--bkgd-height 1)))


(defun fireplace-up ()
  "Move the fire further up."
  (interactive)
  (setq fireplace--bkgd-height (max 0 (- fireplace--bkgd-height 1))))

(defun fireplace-toggle-smoke ()
  "Toggle smoke on/off."
  (interactive)
  (setq fireplace-smoke-on (not fireplace-smoke-on)))

;;; Key-bindings

(define-derived-mode fireplace-mode special-mode  "A cozy fireplace.")

(define-key fireplace-mode-map (kbd "C-+") 'fireplace-down)
(define-key fireplace-mode-map (kbd "C--") 'fireplace-up)
(define-key fireplace-mode-map (kbd "C-*") 'fireplace-toggle-smoke)
(define-key fireplace-mode-map (kbd "q") 'fireplace-off)
(define-key fireplace-mode-map (kbd "Q") 'fireplace-off)

(provide 'fireplace)
;;; fireplace.el ends here
