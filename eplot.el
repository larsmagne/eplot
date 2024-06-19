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
  "Minor mode to issue commands from an eplot data buffer."
  :lighter " eplot")

(defvar-keymap eplot-minor-mode-map
  "C-c C-c" #'eplot-update-view-buffer
  "H-l" #'eplot-eval-and-update)

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
  ;; This is mainly useful during implementation.
  (if (and (eq major-mode 'emacs-lisp-mode)
	   (get-buffer-window "*eplot*" t))
      (with-current-buffer "*eplot*"
	(eplot-update))
    ;; Normal case.
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
	  (eplot--render data))))))

(defun eplot-eval-and-update ()
  "Helper command when developing."
  (interactive nil emacs-lisp-mode)
  (save-some-buffers t)
  (elisp-eval-buffer)
  (eval-defun nil)
  (eplot-update-view-buffer))

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
	      "^\\([0-9.]+\\)[ \t]+\\(\\([0-9.]+\\)[ \t]+\\)?\\(#\\(.*\\)\\)"
	      nil t)
	(let ((v1 (string-to-number (match-string 1)))
	      (v2 (match-string 3))
	      (label (substring-no-properties (match-string 5))))
	  (cond
	   ((and v2 label)
	    (push (list :value v1 :x (string-to-number v2) :label label)
		  values))
	   (v2
	    (push (list :value v1 :x (string-to-number v2)) values))
	   (label
	    (push (list :value v1 :label label) values))
	   (t
	    (push (list :value v1) values)))))
      (push (cons :values (nreverse values)) data)
      data)))

(defun eplot--vn (type data &optional default)
  (if-let ((value (cdr (assq type data))))
      (string-to-number value)
    default))

(defun eplot--vs (type data &optional default)
  (or (cdr (assq type data)) default))

(defun eplot--render (data)
  (let* ((factor (image-compute-scaling-factor))
	 (width (eplot--vn 'width data
			   (* (window-pixel-width
			       (get-buffer-window "*eplot*" t))
			      0.9)))
	 (height (eplot--vn 'height data
			    (* (window-pixel-height
				(get-buffer-window "*eplot*" t))
			       0.9)))
	 (margin-left (* factor (eplot--vn 'margin-left data 50)))
	 (margin-right (* factor (eplot--vn 'margin-right data 20)))
	 (margin-top (* factor (eplot--vn 'margin-top data 50)))
	 (margin-bottom (* factor (eplot--vn 'margin-bottom data 100)))
	 (svg (svg-create width height))
	 (font (eplot--vs 'font data "futural"))
	 (font-size (eplot--vn 'font data (* factor 20)))
	 (color (eplot--vs 'color data "black"))
	 (axes-color (eplot--vs 'axes-color data color))
	 (legend-color (eplot--vs 'legend-color data axes-color)))
    ;; Add background.
    (svg-rectangle svg 0 0 width height
		   :fill (eplot--vs 'background-color data "white"))
    ;; Draw axes.
    (svg-line svg margin-left margin-top margin-left
	      (+ (- height margin-bottom) 10)
	      :stroke axes-color)
    (svg-line svg (- margin-left 10) (- height margin-bottom)
	      (- width margin-right) (- height margin-bottom)
	      :stroke axes-color)
    ;; Title and legends.
    (when-let ((title (eplot--vs 'title data)))
      (svg-text svg title
		:font-family font
		:text-anchor "middle"
		:font-size font-size
		:fill legend-color
		:x (+ margin-left (/ (- width margin-left margin-right) 2))
		:y (+ (* factor 10) (/ margin-top 2))))
    (when-let ((label (eplot--vs 'x-label data)))
      (svg-text svg label
		:font-family font
		:text-anchor "middle"
		:font-size font-size
		:fill legend-color
		:x (+ margin-left (/ (- width margin-left margin-right) 2))
		:y (+ (* factor 4) (- height (/ margin-bottom 4)))))
    (when-let ((label (eplot--vs 'y-label data)))
      (svg-text svg label
		:font-family font
		:text-anchor "middle"
		:font-size font-size
		:fill legend-color
		:transform
		(format "translate(%s,%s) rotate(-90)"
			(+ (/ margin-left 2) (* 7 factor))
			(+ margin-top
			   (/ (- height margin-bottom margin-top) 2)))))
    ;; Analyze values.
    (when-let* ((values (cdr (assq :values data)))
		(vals (seq-map (lambda (v) (plist-get v :value)) values))
		(min (seq-min vals))
		(max (seq-max vals))
		(stride (/ (- width margin-left margin-right)
			   (length vals))))
      ;; Make ticks.
      (cl-loop for elem in values
	       for x from 0
	       for label = (or (plist-get elem :label)
			       (format "%s" (1+ x)))
	       do
	       (svg-line svg
			 (+ margin-left (* x stride) (/ stride 2))
			 (- height margin-bottom)
			 (+ margin-left (* x stride) (/ stride 2))
			 (+ (- height margin-bottom) 10)
			 :stroke legend-color)
	       (svg-text svg label
			 :font-family font
			 :text-anchor "middle"
			 :font-size font-size
			 :fill legend-color
			 :x (+ margin-left (* x stride) (/ stride 2))
			 :y (+ (- height margin-bottom)
			       (* factor 30))))
      (cl-loop for val in vals
	       for x from 0
	       for py = (- (- height margin-bottom)
			   (* (/ (* 1.0 val) max)
			      (- height margin-bottom margin-top)))
	       do
	       (svg-line svg
			 (+ margin-left (* x stride))
			 (- height margin-bottom)
			 (+ margin-left (* x stride))
			 py
			 :stroke color)
	       (svg-line svg
			 (+ margin-left (* x stride))
			 py
			 (+ margin-left (* x stride) stride)
			 py
			 :stroke color)
	       (svg-line svg
			 (+ margin-left (* x stride) stride)
			 py
			 (+ margin-left (* x stride) stride)
			 (- height margin-bottom)
			 :stroke color))
      
      )

    
    (let ((image-scaling-factor 1))
      (svg-insert-image svg))
    ))

(provide 'eplot)

;;; eplot.el ends here
