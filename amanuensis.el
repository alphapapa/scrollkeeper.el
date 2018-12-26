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

;; This package provides scrolling commands and several customization
;; options.  The scrolling commands use `pulse' to display a quickly
;; fading guideline on the line at which new contents are visible
;; after scrolling, helping your eyes to keep their place in the
;; buffer.

;;;; Credits

;; Inspired by Clemens Radermacher's blog post,
;; <https://with-emacs.com/posts/keep-scrollin-scrollin-scrollin/>.

;;;; Prior art

;; TODO: Review and compare these.  I don't think they do quite the
;; same thing, but I'm not sure.

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

(defcustom amanuensis-scroll-distance 0.75
  "How far to scroll."
  :type '(choice (integer :tag "Number of lines")
                 (float :tag "Ratio of window size")))

(defcustom amanuensis-contents-pulse-interval 0.05
  "How often to update guideline pulse steps, in seconds."
  :type 'float)

(defcustom amanuensis-contents-pulse-steps 10
  "Number of steps for guideline pulses."
  :type 'integer)

(defcustom amanuensis-guideline-fn #'amanuensis--highlight-line
  "Function used to display the guideline."
  :type '(choice (const :tag "Highlight line" amanuensis--highlight-line)
                 (const :tag "Insert thin line" amanuensis--insert-line)))

;;;; Faces

(defface amanuensis-guideline-highlight
  ;; FIXME: I picked `font-lock-string-face' because it looks nice
  ;; with my theme.  Maybe not the best default.
  `((t :background ,(face-attribute 'font-lock-string-face :foreground)))
  "Face for highlighting scrolling guideline.")

(defface amanuensis-guideline-thinline
  ;; FIXME: 0.1 is still not as thin as I would like, but I don't know
  ;; if it's possible to make it thinner.
  `((t :height 0.1 :background "red"))
  "Face for thinline guideline.")

;;;; Commands

(cl-defun amanuensis-scroll-contents-down (&optional (lines amanuensis-scroll-distance))
  "Scroll page contents down by LINES, displaying a guideline.
LINES may be an integer number of lines or a float ratio of
window height; see `amanuensis-scroll-distance'."
  (interactive)
  (setq lines (* -1 lines))
  (let ((lines (cl-typecase lines
                 (integer lines)
                 (float (floor (* lines (window-text-height))))))
        (pulse-delay amanuensis-contents-pulse-interval)
        (pulse-iterations amanuensis-contents-pulse-steps))
    (save-excursion
      (move-to-window-line (if (< lines 0)
                               0
                             -1))
      (funcall amanuensis-guideline-fn))
    (scroll-up lines)))

(cl-defun amanuensis-scroll-contents-up (&optional (lines amanuensis-scroll-distance))
  "Scroll page contents up by LINES, displaying a guideline.
LINES may be an integer number of lines or a float ratio of
window height; see `amanuensis-scroll-distance'."
  (interactive)
  (amanuensis-scroll-contents-down (* -1 lines)))

;;;; Functions

(defun amanuensis--highlight-line ()
  "Pulse-highlight the line at point."
  (pulse-momentary-highlight-one-line (point) 'amanuensis-guideline-highlight))

(defun amanuensis--insert-line ()
  "Pulse-highlight a thin line between lines."
  ;; Like `pulse-momentary-highlight-region'.
  (save-excursion
    (let ((o (make-overlay (line-beginning-position) (line-beginning-position))))
      (overlay-put o 'pulse-delete t)
      (overlay-put o 'before-string (propertize "\n" 'face 'amanuensis-guideline-thinline))
      (pulse-momentary-highlight-overlay o 'amanuensis-guideline-thinline))))

;;;; Footer

(provide 'amanuensis)

;;; amanuensis.el ends here
