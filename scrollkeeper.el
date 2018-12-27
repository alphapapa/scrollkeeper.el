;;; scrollkeeper.el --- Configurable scrolling commands with visual guidelines  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/scrollkeeper.el
;; Keywords: convenience
;; Version: 0.1-pre
;; Package-Requires: ((emacs "24"))

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
;; options.  The commands use `pulse' to display a quickly fading
;; guideline on the line at which new contents are visible after
;; scrolling.  Also, scrolling can be divided into adjustable steps at
;; the desired speed.  Together, these features help your eyes to keep
;; their place in the buffer while scrolling.

;; To use this package, simply bind these commands to your preferred
;; keys:

;; + `scrollkeeper-up'
;; + `scrollkeeper-down'

;;;; Credits

;; + Inspired by Clemens Radermacher's blog post, <https://with-emacs.com/posts/keep-scrollin-scrollin-scrollin/>.
;; + Aided by studying Michael Heerdegen's package, <https://github.com/michael-heerdegen/on-screen.el>.

;;;; See also

;; These packages provide some similar functionality but in very different ways.

;; + https://github.com/michael-heerdegen/on-screen.el: A more complex
;; and comprehensive implementation that uses hooks to observe
;; scrolling in other windows.

;; + https://github.com/ska2342/highlight-context-line/: Highlights
;; the boundary line statically, using a minor mode rather than
;; commands.

;; + https://github.com/Malabarba/beacon: Highlights the cursor rather
;; than the boundary line between new and old content.

;;; TODOs:

;; MAYBE: Use different faces for scrolling down and up.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'pulse)

;;;; Customization

(defgroup scrollkeeper nil
  "Scroll with a helpful guideline."
  :link '(url-link "https://github.com/alphapapa/scrollkeeper.el")
  :group 'convenience)

(defcustom scrollkeeper-scroll-distance 0.75
  "Scroll this far with each command.
This may be let-bound in custom commands, or buffer-locally."
  :type '(choice (integer :tag "Number of lines")
                 (float :tag "Ratio of window size")))

(defcustom scrollkeeper-scroll-steps 3
  "Scroll in this many steps.
The lines computed from `scrollkeeper-scroll-distance' are divided
into this many steps.

In a heavily font-locked buffer, scrolling may be slower, so this
variable could be set buffer-locally to a lower value."
  :type 'integer)

(defcustom scrollkeeper-scroll-step-delay 0.001
  "Scroll one step ahead at this interval, in seconds."
  :type 'float)

(defcustom scrollkeeper-guideline-pulse-interval 0.05
  "Step through guideline pulsing at this interval, in seconds."
  :type 'float)

(defcustom scrollkeeper-guideline-pulse-steps 10
  "Divide guideline pulsing into this many steps."
  :type 'integer)

(defcustom scrollkeeper-guideline-fn #'scrollkeeper--highlight
  "Display the guideline with this function."
  :type '(choice (const :tag "Highlight line" scrollkeeper--highlight)
                 (const :tag "Underline line" scrollkeeper--underline)
                 (const :tag "Insert thin line" scrollkeeper--thinline)))

;;;; Faces

;; FIXME: I picked `font-lock-string-face' because it looks nice with
;; my theme.  Maybe not the best default.

(defface scrollkeeper-guideline-highlight
  `((t :background ,(face-attribute 'font-lock-string-face :foreground)))
  "Face for highlighting scrolling guideline.")

(defface scrollkeeper-guideline-thinline
  ;; FIXME: 0.1 is still not as thin as I would like, but I don't know
  ;; if it's possible to make it thinner.
  `((t :height 0.1 :background ,(face-attribute 'font-lock-string-face :foreground)))
  "Face for thinline guideline.")

(defface scrollkeeper-guideline-underline
  ;; Thanks to `on-screen-narrow-line' for showing how to use the
  ;; `extra-expanded' value for `:width'.  It doesn't seem to be
  ;; mentioned in the Elisp manual, so it's not easy to discover.
  `((t :width extra-expanded
       :underline (:color ,(face-attribute 'font-lock-string-face :foreground) :style wave)))
  "Face for underline guideline.")

;;;; Commands

;;;###autoload
(cl-defun scrollkeeper-contents-up (&optional (lines scrollkeeper-scroll-distance))
  "Scroll page contents up by LINES, displaying a guideline.
LINES may be an integer number of lines or a float ratio of
window height; see `scrollkeeper-scroll-distance'."
  (interactive)
  (let* ((lines (cl-typecase lines
                  (integer lines)
                  (float (floor (* lines (window-text-height))))))
         (steps (floor (/ lines scrollkeeper-scroll-steps)))
         (pulse-delay scrollkeeper-guideline-pulse-interval)
         (pulse-iterations scrollkeeper-guideline-pulse-steps))
    (save-excursion
      (move-to-window-line (if (< lines 0)
                               0
                             -1))
      (funcall scrollkeeper-guideline-fn))
    (dotimes (_ scrollkeeper-scroll-steps)
      (scroll-up steps)
      (sit-for scrollkeeper-scroll-step-delay))))

;;;###autoload
(defalias 'scrollkeeper-down #'scrollkeeper-contents-up)

;;;###autoload
(cl-defun scrollkeeper-contents-down (&optional (lines scrollkeeper-scroll-distance))
  "Scroll page contents down by LINES, displaying a guideline.
LINES may be an integer number of lines or a float ratio of
window height; see `scrollkeeper-scroll-distance'."
  (interactive)
  (scrollkeeper-contents-up (* -1 lines)))

;;;###autoload
(defalias 'scrollkeeper-up #'scrollkeeper-contents-down)

;;;; Functions

(defun scrollkeeper--highlight ()
  "Pulse-highlight the line at point."
  (pulse-momentary-highlight-one-line (point) 'scrollkeeper-guideline-highlight))

(defun scrollkeeper--thinline ()
  "Pulse-highlight a thin line between lines."
  ;; Like `pulse-momentary-highlight-region'.
  (save-excursion
    (let ((o (make-overlay (line-beginning-position) (line-beginning-position))))
      (overlay-put o 'pulse-delete t)
      (overlay-put o 'before-string (propertize "\n" 'face 'scrollkeeper-guideline-thinline))
      (pulse-momentary-highlight-overlay o 'scrollkeeper-guideline-thinline))))

(defun scrollkeeper--underline ()
  "Pulse-highlight an underline overlay on the line at point."
  ;; Like `pulse-momentary-highlight-region'.
  (save-excursion
    (let ((o (make-overlay (line-beginning-position) (line-end-position))))
      (overlay-put o 'pulse-delete t)
      ;; Thanks to `on-screen-make-narrow-line-overlay' for showing
      ;; how to get the details of the `space' `display' property
      ;; right.
      (overlay-put o 'after-string
                   (propertize " "
                               'face 'scrollkeeper-guideline-underline
                               'display `(space :align-to ,(window-width))
                               ;; FIXME: Not sure if `cursor' is necessary here.  Doesn't seem to have any effect.
                               'cursor 0))
      (pulse-momentary-highlight-overlay o 'scrollkeeper-guideline-underline))))

;;;; Footer

(provide 'scrollkeeper)

;;; scrollkeeper.el ends here
