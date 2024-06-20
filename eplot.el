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
    ;; First headers.
    (let ((data (eplot--parse-headers))
	  plots)
      ;; Then the values.
      (while-let ((plot (eplot--parse-values)))
	(push plot plots))
      (when plots
	(push (cons :plots (nreverse plots)) data))
      data)))

(defun eplot--parse-headers ()
  (let ((data nil)
	type value)
    (while (looking-at "\\([^\n\t :]+\\):\\(.*\\)")
      (setq type (intern (downcase (match-string 1)))
	    value (string-trim (match-string 2)))
      (forward-line 1)
      ;; Get continuation lines.
      (while (looking-at "[ \t]+\\(.*\\)")
	(setq value (concat value " " (string-trim (match-string 1))))
	(forward-line 1))
      (push (cons type value) data))
    data))

(defun eplot--parse-values ()
  ;; Skip past separator lines.
  (while (looking-at "[ \t]*\n")
    (forward-line 1))
  (let ((values nil)
	;; We may have plot-specific headers.
	(headers (eplot--parse-headers)))
    (if-let ((data-file (eplot--vs 'data headers)))
	(with-temp-buffer
	  (insert-file-contents data-file)
	  (setq values (cdr (assq :values (eplot--parse-values)))
		headers (delq (assq 'data headers) headers)))
      ;; Now we come to the data.  The data is typically either just a
      ;; number, or two numbers (in which case the first number is a
      ;; date or a time).  Labels can be introduced with a # char.
      (while (looking-at
	      "^[ \t]*\\([0-9.]+\\)\\([ \t]+\\([0-9.]+\\)\\)?\\([ \t]+#\\(.*\\)\\)?")
	(let ((v1 (string-to-number (match-string 1)))
	      (v2 (match-string 3))
	      (label (match-string 5)))
	  (cond
	   ((and v2 label)
	    (push (list :value v1 :x (string-to-number v2)
			:label (substring-no-properties label))
		  values))
	   (v2
	    (push (list :value v1 :x (string-to-number v2)) values))
	   (label
	    (push (list :value v1 :label (substring-no-properties label))
		  values))
	   (t
	    (push (list :value v1) values))))
	(forward-line 1))
      (setq values (nreverse values)))
    (and values
	 `((:headers . ,headers) (:values . ,values)))))

(defun eplot--vn (type data &optional default)
  (if-let ((value (cdr (assq type data))))
      (string-to-number value)
    default))

(defun eplot--vs (type data &optional default)
  (or (cdr (assq type data)) default))

(defun eplot--vy (type data &optional default)
  (if-let ((value (cdr (assq type data))))
      (intern value)
    default))

(defun eplot--render (data)
  (let* ((factor (image-compute-scaling-factor))
	 (width (eplot--vn 'width data
			   (/ (* (window-pixel-width
 				  (get-buffer-window "*eplot*" t))
				 0.9)
			      factor)))
	 (height (eplot--vn 'height data
			    (/ (* (window-pixel-height
				   (get-buffer-window "*eplot*" t))
				  0.9)
			       factor)))
	 (margin-left (eplot--vn 'margin-left data 30))
	 (margin-right (eplot--vn 'margin-right data 10))
	 (margin-top (eplot--vn 'margin-top data 20))
	 (margin-bottom (eplot--vn 'margin-bottom data 21))
	 (style (eplot--vy 'style data 'line))
	 (svg (svg-create width height))
	 (font (eplot--vs 'font data "futural"))
	 (font-size (eplot--vn 'font data 12))
	 (xs (- width margin-left margin-right))
	 (ys (- height margin-top margin-bottom))
	 (color (eplot--vs 'color data "black"))
	 (axes-color (eplot--vs 'axes-color data color))
	 (grid-color (eplot--vs 'grid-color data "#e0e0e0"))
	 (grid-position (eplot--vy 'grid-position data 'bottom))
	 (legend-color (eplot--vs 'legend-color data axes-color)))
    ;; Add background.
    (svg-rectangle svg 0 0 width height
		   :fill (eplot--vs 'background-color data "white"))
    (when-let ((frame-color (eplot--vs 'frame-color data)))
      (svg-rectangle svg 0 0 width height
		     :fill frame-color)
      (svg-rectangle svg margin-left margin-top
		     xs ys
		     :fill (eplot--vs 'background-color data "white")))
    (when-let ((border-color (eplot--vs 'border-color data)))
      (svg-rectangle svg 0 0 width height
		     :stroke-width (eplot--vn 'border-width data 1)
		     :fill "none"
		     :stroke-color border-color))
    ;; Title and legends.
    (when-let ((title (eplot--vs 'title data)))
      (svg-text svg title
		:font-family font
		:text-anchor "middle"
		:font-size font-size
		:fill (eplot--vs 'title-color data legend-color)
		:x (+ margin-left (/ (- width margin-left margin-right) 2))
		:y (+ 3 (/ margin-top 2))))
    (when-let ((label (eplot--vs 'x-label data)))
      (svg-text svg label
		:font-family font
		:text-anchor "middle"
		:font-size font-size
		:fill legend-color
		:x (+ margin-left (/ (- width margin-left margin-right) 2))
		:y (- height (/ margin-bottom 4))))
    (when-let ((label (eplot--vs 'y-label data)))
      (svg-text svg label
		:font-family font
		:text-anchor "middle"
		:font-size font-size
		:fill legend-color
		:transform
		(format "translate(%s,%s) rotate(-90)"
			(- (/ margin-left 2) 2)
			(+ margin-top
			   (/ (- height margin-bottom margin-top) 2)))))
    ;; Analyze values.
    (let* ((values (cdr (assq :values (car (cdr (assq :plots data))))))
	   (vals (seq-map (lambda (v) (plist-get v :value)) values))
	   (min (eplot--vn 'min data (if vals (seq-min vals) 0)))
	   (max (eplot--vn 'max data (if vals (seq-max vals) 0)))
	   (whole (memq style '(impulse bar)))
	   (stride (e/ xs
		       ;; Fenceposting impulse/bar vs everything else.
		       (if (memq style '(impulse bar))
			   (length vals)
			 (1- (length vals)))))
	   (x-ticks (eplot--get-ticks 0 (length values) xs whole))
	   (y-ticks (eplot--get-ticks min max ys))
	   ;; This is how often we should output labels on the ticks.
	   (step (ceiling (e/ (length x-ticks) (e/ width 70)))))

      ;; We may be extending the bottom of the chart to get pleasing
      ;; numbers.  We don't want to be drawing the chart on top of the
      ;; X axis, because the chart won't be visible there.
      (when (<= min (car y-ticks))
	(setq min (- (car y-ticks)
		     ;; 2% of the value range.
		     (* 0.02 (- (car (last y-ticks)) (car y-ticks))))))
      
      (when (eq grid-position 'top)
	(eplot--draw-plots data color style height margin-bottom margin-left
			   min max ys stride svg))      

      ;; Make X ticks.
      (cl-loop for x in x-ticks
	       for label = (if whole
			       (plist-get (elt values x) :label)
			     (format "%s" x))
	       for px = (if (memq style '(impulse bar))
			    (+ margin-left (* x stride) (/ stride 2))
			  (+ margin-left (* x stride)))
	       ;; We might have one extra stride outside the area -- don't
	       ;; draw it.
	       when (> px (- width margin-right))
	       return nil
	       do (svg-line svg
			    px
			    (- height margin-bottom)
			    px
			    (+ (- height margin-bottom)
			       (if (zerop (e% x step))
				   4
				 2))
			    :stroke legend-color)
	       (svg-line svg px margin-top
			 px (- height margin-bottom)
			 :stroke grid-color)
	       when (zerop (e% x step))
	       do (svg-text svg label
			    :font-family font
			    :text-anchor "middle"
			    :font-size font-size
			    :fill legend-color
			    :x px
			    :y (+ (- height margin-bottom)
				  font-size 2)))
      ;; Make Y ticks.
      (let* ((ideal (e/ ys font-size))
	     factor val-factor series spacing offset)
	(if (> ideal (length y-ticks))
	    (setq factor 0.1
		  val-factor 0.1)
	  (let ((please (eplot--pleasing-numbers
			 (ceiling (e/ (length y-ticks) ideal)))))
	    (setq factor (car please)
		  series (cadr please))
	    ;; If we get a too big factor here, we decrease it.
	    (when (< (e/ (length y-ticks) factor) 2)
	      (let ((please (eplot--pleasing-numbers
			     (ceiling (e/ (length y-ticks) ideal 2)))))
		(setq factor (car please)
		      series (cadr please))))
	    (setq val-factor (car (eplot--pleasing-numbers
				   (e/ (- max min) 100)
				   series)))))
	(setq spacing (if (length> y-ticks 1)
			  (abs (- (elt y-ticks 1) (elt y-ticks 0)))
			0))
	(cl-loop for i from 0 upto (* (length y-ticks) 2)
		 for y = (+ (elt y-ticks 0) (* spacing i))
		 when (zerop (mod (truncate (* y 1000))
				  (truncate (* factor 1000))))
		 return (setq offset (e/ (mod i factor) 1000)))
	(cl-loop with iy = 0
		 for y in y-ticks
		 for i from 0
		 for py = (- (- height margin-bottom)
			     (* (/ (- (* 1.0 y) min) (- max min))
				ys))
		 when (> i offset)
		 do (cl-incf iy)
		 do (svg-line svg margin-left py
			      (- margin-left 3) py
			      :stroke-color axes-color)
		 (svg-line svg margin-left py
			   (- width margin-right) py
			   :stroke-color grid-color)
		 when (and (> iy -1)
			   (zerop (% (truncate (* iy 1000))
				     (truncate (* factor 1000)))))
		 do (svg-text svg (format "%s" y)
			      :font-family font
			      :text-anchor "end"
			      :font-size font-size
			      :fill legend-color
			      :x (- margin-left 4)
			      :y (+ py (/ font-size 2) -2))))
      
      ;; Draw axes.
      (svg-line svg margin-left margin-top margin-left
		(+ (- height margin-bottom) 5)
		:stroke axes-color)
      (svg-line svg (- margin-left 5) (- height margin-bottom)
		(- width margin-right) (- height margin-bottom)
		:stroke axes-color)

      (when (eq grid-position 'bottom)
	(eplot--draw-plots data color style height margin-bottom margin-left
			   min max ys stride svg)))
    
    (svg-insert-image svg)))

(defun eplot--draw-plots (data color style height
			       margin-bottom margin-left
			       min max ys
			       stride svg)
  ;; Draw all the plots.
  (cl-loop for plot in (reverse (cdr (assq :plots data)))
	   for headers = (cdr (assq :headers plot))
	   for values = (cdr (assq :values plot))
	   for vals = (seq-map (lambda (v) (plist-get v :value)) values)
	   do
	   (cl-loop
	    with color = (eplot--vs 'color headers color)
	    with style = (eplot--vy 'style headers style)
	    with lpy
	    with lpx
	    for val in vals
	    for x from 0
	    for py = (- (- height margin-bottom)
			(* (/ (- (* 1.0 val) min) (- max min))
			   ys))
	    for px = (+ margin-left (* x stride))
	    do
	    (cl-case style
	      (bar
	       (svg-rectangle svg
			      px py
			      stride (- height margin-bottom py)
			      :stroke color
			      :fill "black"))
	      (impulse
	       (svg-line svg
			 px py
			 px (- height margin-bottom)
			 :stroke color))
	      (point
	       (svg-line svg px py (1+ px) (1+ py)
			 :stroke color))
	      (line
	       (when lpx
		 (svg-line svg lpx lpy px py
			   :stroke color)))
	      (circle)
	      (cross)
	      (filled-square)
	      (triangle)
	      (box)
	      )
	    (setq lpy py
		  lpx px))))

(defun e% (num1 num2)
  (let ((factor (max (expt 10 (eplot--decimal-digits num1))
		     (expt 10 (eplot--decimal-digits num2)))))
    (% (truncate (* num1 factor)) (truncate (* num2 factor)))))

(defun eplot--decimal-digits (number)
  (- (length (replace-regexp-in-string
	      "0+\\'" ""
	      (format "%.10f" (- number (truncate number)))))
     2))

(defun e/ (&rest numbers)
  (if (cl-every #'integerp numbers)
      (let ((int (apply #'/ numbers))
	    (float (apply #'/ (* 1.0 (car numbers)) (cdr numbers))))
	(if (= int float)
	    int
	  float))
    (apply #'/ numbers)))

(defun eplot--get-ticks (min max height &optional whole)
  (let* ((diff (abs (- min max)))
	 (even (car (eplot--pleasing-numbers (* (e/ diff height) 10))))
	 (factor (max (expt 10 (eplot--decimal-digits even))
		      (expt 10 (eplot--decimal-digits diff))))
	 (fmin (truncate (* min factor)))
	 (feven (truncate (* factor even)))
	 start)
    (when whole
      (setq even 1
	    feven factor))

    (setq start
	  (cond
	   ((< min 0)
	    (+ (floor fmin)
	       feven
	       (- (% (floor fmin) feven))
	       (- feven)))
	   (t
	    (- fmin (% fmin feven)))))
    (cl-loop for x from start upto (* max factor) by feven
	     collect (e/ x factor))))

(defun eplot--int (number)
  (cond
   ((integerp number)
    number)
   ((= number (truncate number))
    (truncate number))
   (t
    number)))

(defun eplot--pleasing-numbers (number &optional series)
  (let* ((digits (eplot--decimal-digits number))
	 (one (if (zerop digits) 1 (/ 1.0 (expt 10 digits))))
	 (two (if (zerop digits) 2 (/ 2.0 (expt 10 digits))))
	 (five (if (zerop digits) 5 (/ 5.0 (expt 10 digits)))))
    (catch 'found
      (while t
	(when (and (< number one)
		   (or (null series) (= series 1)))
	  (throw 'found (list one 1)))
	(setq one (* one 10))
	(when (and (< number two)
		   (or (null series) (= series 2)))
	  (throw 'found (list two 2)))
	(setq two (* two 10))
	(when (and (< number five)
		   (or (null series) (= series 5)))
	  (throw 'found (list five 5)))
	(setq five (* five 10))))))

(provide 'eplot)

;;; eplot.el ends here

;;; Todo:
;; Format: compact/default
