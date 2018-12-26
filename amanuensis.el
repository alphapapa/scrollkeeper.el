;;; amanuensis.el --- Guiding your scrolling  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;;; Credits

;; Inspired by Clemens Radermacher's blog post,
;; <https://with-emacs.com/posts/keep-scrollin-scrollin-scrollin/>.

;;;; Prior art

;; +  =on-screen=: https://github.com/michael-heerdegen/on-screen.el
;; +  =highlight-context-line=: https://github.com/ska2342/highlight-context-line/
;; +  =beacon=: https://github.com/Malabarba/beacon Not the same, but also
;;    helpful when scrolling--but probably not when using this package,
;;    as they would surely be seizure-inducing.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'pulse)

;;;; Customization

(defgroup amanuensis nil
  "Scroll with a helpful guideline."
  :link '(url-link "https://github.com/alphapapa/amanuensis.el")
  :group 'convenience)

(defcustom amanuensis-scroll-amount 0.75
  "How far to scroll."
  :type '(choice (integer :tag "Number of lines")
                 (float :tag "Ratio of window size")))

(defcustom amanuensis-contents-pulse-interval 0.025
  "How often to update guideline pulse steps in seconds."
  :type 'float)

(defcustom amanuensis-contents-pulse-steps 10
  "Number of steps for guideline pulses."
  :type 'integer)

;;;; Faces

(defface amanuensis-contents-guideline
  `((t :background ,(face-attribute 'font-lock-string-face :foreground)))
  "Face for scrolling guideline.")

;;;; Commands

(cl-defun amanuensis-scroll-contents-down (&optional (lines amanuensis-scroll-amount))
  "Scroll page contents down by LINES, displaying a guideline.
LINES may be an integer number of lines or a float ratio of
window height; see `amanuensis-scroll-amount'."
  (interactive)
  (setq lines (* -1 lines ))
  (let ((lines (cl-typecase lines
                 (integer lines)
                 (float (floor (* lines (window-text-height))))))
        (pulse-delay amanuensis-contents-pulse-interval)
        (pulse-iterations amanuensis-contents-pulse-steps))
    (save-excursion
      (move-to-window-line (if (< lines 0)
                               0
                             -1))
      (cl-letf (((symbol-function 'pulse-momentary-highlight-overlay)
                 (symbol-function 'amanuensis--pulse-momentary-highlight-overlay)))
        (pulse-momentary-highlight-one-line (point) 'amanuensis-contents-guideline)))
    (scroll-up lines)))

(cl-defun amanuensis-scroll-contents-up (&optional (lines amanuensis-scroll-amount))
  "Scroll page contents up by LINES, displaying a guideline.
LINES may be an integer number of lines or a float ratio of
window height; see `amanuensis-scroll-amount'."
  (interactive)
  (amanuensis-scroll-contents-down (* -1 lines)))

;;;; Functions

(defun amanuensis--pulse-momentary-highlight-overlay (o &optional face)
  "Pulse the overlay O.
Optional argument FACE specifies the face to do the highlighting.

Like `pulse-momentary-highlight-overlay', but does not clear on
next command."
  ;; We don't support simultaneous highlightings.
  (pulse-momentary-unhighlight)
  (overlay-put o 'original-face (overlay-get o 'face))
  (setq pulse-momentary-overlay o)
  (if (eq pulse-flag 'never)
      nil
    (if (or (not pulse-flag) (not (pulse-available-p)))
	;; Provide a face
	(overlay-put o 'face (or face 'pulse-highlight-start-face))
      ;; Pulse it.
      (overlay-put o 'face 'pulse-highlight-face)
      ;; The pulse function puts FACE onto 'pulse-highlight-face.
      ;; Thus above we put our face on the overlay, but pulse
      ;; with a reference face needed for the color.
      (pulse-reset-face face)
      (setq pulse-momentary-timer
            (run-with-timer 0 pulse-delay #'pulse-tick
                            (time-add (current-time)
                                      (* pulse-delay pulse-iterations)))))))

;;;; Footer

(provide 'amanuensis)

;;; amanuensis.el ends here
