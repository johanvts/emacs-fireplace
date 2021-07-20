;;; fireplace.el --- A cozy fireplace for emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Johan Sivertsen
;;; Version: 1.2.0
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

;; Oh, the weather outside is frightful!
;; But the fire is so delightful...

;;; Code:

(defgroup fireplace nil
  "Config for `fireplace'."
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

(defcustom fireplace-sound-on nil
  "Controls if it plays sound or not."
  :type 'string :group 'fireplace)

(defcustom fireplace-sound-file-path (concat (file-name-directory load-file-name) "fireplace.mp3")
  "Default path for fireplace sound file."
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

(defvar fireplace--bkgd-height nil
  "Used for fireplace height, will be set from windows size")

(defvar fireplace--bkgd-width nil
  "Used for fireplace width, will be set from windows size")

(defvar fireplace--timer nil
  "Holds the active fireplace, kill using fireplace-off")

(defvar fireplace--flame-width nil
  "Calculated width of flames")

(defvar fireplace--flame-pos nil
  "Flame position")

(defvar fireplace--sound-process nil
  "Holds sound process object, used to kill sound process")

;;; Helper routines

(defun fireplace--make-grid ()
  "Redraw backgound of buffer."
  (erase-buffer)
  (dotimes (_ fireplace--bkgd-height)
    (insert-char fireplace-background-char fireplace--bkgd-width)
    (newline)))

(defun fireplace--gotoxy (x y)
  "Move pointer to position X Y."
  (goto-char (+ 1 x (* (- fireplace--bkgd-height (+ 1 y))
                       (+ 1 fireplace--bkgd-width)))))

(defun fireplace--draw-flame-stripe (x y width)
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
      (fireplace--draw-flame-stripe x y width))
    (dotimes (y high)
      (setq line (+ lower y)
            width (max 0 (- width 1 (random 3)))
            x (- middle (/ width 2)))
      (when (< x 0)
        (setq width (+ width x)
              x 0))
      (when (> (+ x width) fireplace--bkgd-width)
        (setq width (- fireplace--bkgd-width x)))
      (fireplace--draw-flame-stripe x line width)
      (when fireplace-smoke-on (fireplace--smoke x h)))))

(defun fireplace-draw (buffer-name)
  "Draw the whole fireplace in BUFFER-NAME from FLAME-POS with FLAME-WIDTH."
  (with-current-buffer (get-buffer-create buffer-name)
    (if (not (eq major-mode 'fireplace-mode))
        (fireplace-off)
      (setq buffer-read-only nil)
      (fireplace--make-grid)
      (dolist (pos fireplace--flame-pos)
        (fireplace--flame (round (* pos fireplace--bkgd-width))
                          (+ (round (* (+ 0.2 (min pos (- 1 pos))) fireplace--flame-width))
                             (random 3))))
      (setq buffer-read-only t))))

(defun fireplace--disable-minor-modes ()
  "Disable minor modes that might affect rendering."
  (switch-to-buffer fireplace-buffer-name)
  ;; Use local variables to avoid messing with the actual editing enviornment
  (setq-local truncate-lines t
              cursor-type nil
              show-trailing-whitespace nil
              indicate-empty-lines nil
              transient-mark-mode nil
              hl-line-mode nil
              ;; global-hl-line mode overrides the local hl-line-mode
              ;; *for some reason* and it's still called global-hl-line-mode
              ;; *even though* you can set 'global-hl-line-mode' as a buffer-local.
              global-hl-line-mode nil
              ;; non-standard emacs packages
              beacon-mode nil
              )
  ;; Reference the fireplace buffer in-case the current buffer
  ;; isn't the fireplace, for some reason.
  (buffer-disable-undo fireplace-buffer-name)
  )
(defun fireplace--update-locals-vars (&optional stub-window)
  "Update `fireplace' local variables."
  (setq fireplace--bkgd-height (- (floor (window-height (get-buffer-window fireplace-buffer-name))) 1)
        fireplace--bkgd-width  (- (round (window-width (get-buffer-window fireplace-buffer-name))) 1)
        fireplace--flame-width (min fireplace--bkgd-height (round (/ fireplace--bkgd-width 2.5)))
        fireplace--flame-pos fireplace-flame-pos))

(defun fireplace--extinguish-flames ()
  "Cancel the `fireplace-draw' timer."
  (cancel-function-timers 'fireplace-draw))

(defun fireplace--play-sound ()
  "Play fireplace sound in a loop."
  (if (executable-find "ffplay")
      (progn (setq fireplace--sound-process
                   (start-process "fireplace-sound" nil
                                  "ffplay" "-nodisp" "-nostats" "-hide_banner" "-loop" "0"
                                  fireplace-sound-file-path))
             ;; Kill sound process before kill-buffer
             (setq-local kill-buffer-query-functions '(fireplace--stop-sound)))
    (message "Executable not found: \"ffplay\"")))

(defun fireplace--stop-sound ()
  "Stop fireplace sound."
  ;; There is no process if executable not found.
  ;; Should always return non-nil to not prevent kill-buffer. See `kill-buffer-query-functions'
  (if (process-live-p fireplace--sound-process)
      (kill-process fireplace--sound-process) t))

;; Commands

;;;###autoload
(defun fireplace ()
  "Light the fire."
  (interactive)
  (with-current-buffer (get-buffer-create fireplace-buffer-name)
    (fireplace--update-locals-vars)
    (fireplace--make-grid)
    (fireplace-mode)
    (add-hook 'window-size-change-functions 'fireplace--update-locals-vars nil t)
    (fireplace--disable-minor-modes)
    (when fireplace-sound-on (fireplace--play-sound))
    (run-with-timer 1 (- 1 fireplace-fury) 'fireplace-draw fireplace-buffer-name)))

(defun fireplace-off ()
  "Put out the fire."
  (interactive)
  (fireplace--extinguish-flames)
  (when (get-buffer fireplace-buffer-name)
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

(defun fireplace-toggle-sound ()
  "Toggle sound on/off."
  (interactive)
  (if fireplace-sound-on (fireplace--stop-sound) (fireplace--play-sound))
  (setq fireplace-sound-on (not fireplace-sound-on)))

;;; Key-bindings

(defvar fireplace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-+") 'fireplace-up)
    (define-key map (kbd "C--") 'fireplace-down)
    (define-key map (kbd "C-*") 'fireplace-toggle-smoke)
    (define-key map (kbd "C-=") 'fireplace-toggle-sound)
    (define-key map (kbd "q") 'fireplace-off)
    (define-key map (kbd "Q") 'fireplace-off)
    map)
  "Keymap for `fireplace-mode'.")

(define-derived-mode fireplace-mode special-mode  "A cozy fireplace."
  "Major mode for *fireplace* buffers.")

(provide 'fireplace)
;;; fireplace.el ends here
