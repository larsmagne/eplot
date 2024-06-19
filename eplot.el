;;; eplot.el --- Manage and Edit Wordpress Posts -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: charts
;; Package: eplot
;; Version: 1.0
;; Package-Requires: ((emacs "29.0.59"))

;; eplot is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; The main entry point is `M-x eplot' in a buffer with time series
;; data.

;;; Code:

(require 'cl-lib)

(define-minor-mode eplot-minor-mode
  "Minor mode to issue commands from an eplot data buffer.")

(defvar-keymap eplot-minor-mode-map
  "C-c C-c" #'eplot-update-view-buffer)

(define-derived-mode eplot-mode special-mode "eplot"
  "Major mode for displaying eplots.")

(defvar-keymap eplot-mode-map
  "g" #'eplot-update)

(defvar eplot--data-buffer nil)

(defun eplot ()
  "Plot the data in the current buffer."
  (interactive)
  (eplot-minor-mode 1)
  (eplot-update-view-buffer))

(defun eplot-update-view-buffer ()
  "Update the eplot view buffer based on the current data buffer."
  (interactive)
  (let ((data (eplot--parse-buffer))
	(data-buffer (current-buffer)))
    (save-current-buffer
      (if (get-buffer-window "*eplot*" t)
	  (set-buffer "*eplot*")
	(pop-to-buffer "*eplot*"))
      (let ((inhibit-read-only t))
	(erase-buffer)
	(unless (eq major-mode 'eplot-mode)
	  (eplot-mode))
	(setq-local eplot--data-buffer data-buffer)
	(eplot--render data)))))

(defun eplot-update ()
  "Update the plot in the current buffer."
  (interactive)
  (let ((data (with-current-buffer eplot--data-buffer
		(eplot--parse-buffer)))
	(inhibit-read-only t))
    (erase-buffer)
    (eplot--render data)))

(defun eplot--parse-buffer ()
  (save-excursion
    (goto-char (point-min))
    (let ((data nil)
	  type value values)
      (while (looking-at "\\([^ :]+\\):\\(.*\\)")
	(setq type (intern (downcase (match-string 1)))
	      value (string-trim (match-string 2)))
	(forward-line 1)
	;; Get continuation lines.
	(while (looking-at "[ \t]+\\(.*\\)")
	  (setq value (concat value " " (string-trim (match-string 1))))
	  (forward-line 1))
	(push (cons type value) data))
      ;; Skip past separator lines.
      (while (looking-at "[ \t]*\n")
	(forward-line 1))
      ;; Now we come to the data.  The data is typically either just a
      ;; number, or two numbers (in which case the first number is a
      ;; date or a time).  Labels can be introduced with a # char.
      (while (re-search-forward
	      "^\\([0-9]+\\)[ \t]+\\(\\([0-9]+\\)[ \t]+\\)?\\(#\\(.*\\)"
	      nil t)
	(let ((v1 (match-string 1))
	      (v2 (match-string 3))
	      (label (match-string 5)))
	  (cond
	   ((and v2 label)
	    (push (list v1 v2 label) values))
	   (v2
	    (push (list v1 v2) values))
	   (t
	    (push v1 values))))
	(push (cons :values (nreverse values)) data))
      data)))

(defun eplot--render (data)
  (let* ((width (or (cdr (assq 'width data))
		    (window-pixel-width (get-buffer-window "*eplot*"))))
	 (height (or (cdr (assq 'height data))
		     (window-pixel-height (get-buffer-window "*eplot*"))))
	 (svg (svg-create width height))
	 (axes-color (or (cdr (assq 'axes-color data)) "black")))
    (svg-rectangle svg 0 0 width height
		   :fill (or (cdr (assq 'background-color data)) "white"))
    ))

(provide 'eplot)

;;; eplot.el ends here
