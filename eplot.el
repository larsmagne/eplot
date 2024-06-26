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

(setq auto-mode-alist (cons '("\\.plt" . eplot-mode) auto-mode-alist))

(defvar-keymap eplot-mode-map
  "C-c C-c" #'eplot-update-view-buffer)

(define-derived-mode eplot-mode text-mode "eplot")

(define-minor-mode eplot-minor-mode
  "Minor mode to issue commands from an eplot data buffer."
  :lighter " eplot")

(defvar-keymap eplot-minor-mode-map
  "H-l" #'eplot-eval-and-update)

(define-derived-mode eplot-view-mode special-mode "eplot view"
  "Major mode for displaying eplots.")

(defvar-keymap eplot-view-mode-map
  "g" #'eplot-update)

(defvar eplot-default-size '(600 400)
  "Default size for plots without a specified size.")

(defvar eplot--data-buffer nil)

(defun eplot ()
  "Plot the data in the current buffer."
  (interactive)
  (eplot-update-view-buffer))

(defun eplot-update-view-buffer ()
  "Update the eplot view buffer based on the current data buffer."
  (interactive)
  ;; This is mainly useful during implementation.
  (if (and (eq major-mode 'emacs-lisp-mode)
	   (get-buffer-window "*eplot*" t))
      (with-current-buffer "*eplot*"
	(eplot-update)
	(when-let ((win (get-buffer-window "*eplot*" t)))
	  (set-window-point win (point-min))))
    ;; Normal case.
    (let ((data (eplot--parse-buffer))
	  (data-buffer (current-buffer)))
      (save-current-buffer
	(if (get-buffer-window "*eplot*" t)
	    (set-buffer "*eplot*")
	  (pop-to-buffer "*eplot*"))
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (unless (eq major-mode 'eplot-view-mode)
	    (eplot-view-mode))
	  (setq-local eplot--data-buffer data-buffer)
	  (eplot--render data)
	  (insert "\n")
	  (when-let ((win (get-buffer-window "*eplot*" t)))
	    (set-window-point win (point-min))))))))

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
    (eplot--render data)
    (insert "\n\n")))

(defun eplot--parse-buffer ()
  (save-excursion
    (goto-char (point-min))
    ;; First headers.
    (let* ((data (eplot--parse-headers))
	   (plot-headers
	    ;; It's OK not to separate the plot headers from the chart
	    ;; headers.  Collect them here, if any.
	    (cl-loop for elem in '( smoothing gradient style fill color size
				    data-format fill-border data)
		     for value = (eplot--vs elem data)
		     when value
		     collect (cons elem value)))
	   plots)
      ;; Then the values.
      (while-let ((plot (eplot--parse-values nil plot-headers)))
	(setq plot-headers nil)
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

(defun eplot--parse-values (&optional in-headers data-headers)
  ;; Skip past separator lines.
  (while (looking-at "[ \t]*\n")
    (forward-line 1))
  (let* ((values nil)
	 ;; We may have plot-specific headers.
	 (headers (nconc (eplot--parse-headers) data-headers))
	 (data-format (or (eplot--vyl 'data-format headers)
			  (eplot--vyl 'data-format in-headers)))
	 (two-values (memq 'two-values data-format))
	 (xy (or (memq 'date data-format)
		 (memq 'time data-format)
		 (memq 'xy data-format)))
	 (data-column (or (eplot--vn 'data-column headers)
			  (eplot--vn 'data-column in-headers))))
    (if-let ((data-file (eplot--vs 'data headers)))
	(with-temp-buffer
	  (insert-file-contents data-file)
	  (setq values (cdr (assq :values (eplot--parse-values headers)))
		headers (delq (assq 'data headers) headers)))
      ;; Now we come to the data.  The data is typically either just a
      ;; number, or two numbers (in which case the first number is a
      ;; date or a time).  Labels ans settings can be introduced with
      ;; a # char.
      (while (looking-at "\\([-0-9. \t]+\\)\\([ \t]+#\\(.*\\)\\)?")
	(let ((numbers (mapcar #'string-to-number
			       (split-string (string-trim (match-string 1)))))
	      (settings (eplot--parse-settings (match-string 3)))
	      this)
	  ;; If we're reading two dimensionalish data, the first
	  ;; number is the date/time/x.
	  (when xy
	    (setq this (list :x (pop numbers))))
	  ;; Chop off all the numbers until we read the column(s)
	  ;; we're using.
	  (when data-column
	    (setq numbers (nthcdr (1- data-column) numbers)))
	  (setq this (nconc this (list :value (pop numbers))))
	  (when two-values
	    (setq this (nconc this (list :extra-value (pop numbers)))))
	  (when settings
	    (nconc this (list :settings settings)))
	  (push this values))
	(forward-line 1))
      (setq values (nreverse values)))
    (and values
	 `((:headers . ,headers) (:values . ,values)))))

(defun eplot--parse-settings (string)
  (when string
    (with-temp-buffer
      (insert (string-trim string) "\n")
      (goto-char (point-min))
      (while (re-search-forward "\\(.\\)," nil t)
	(if (equal (match-string 1) "\\")
	    (replace-match "," t t)
	  (delete-char -1)
	  (insert "\n")
	  (when (looking-at "[ \t]+")
	    (replace-match ""))))
      (goto-char (point-min))
      (eplot--parse-headers))))

(defun eplot--vn (type data &optional default)
  (if-let ((value (cdr (assq type data))))
      (string-to-number value)
    default))

(defun eplot--vs (type data &optional default)
  (or (cdr (assq type data)) default))

(defun eplot--vy (type data &optional default)
  (if-let ((value (cdr (assq type data))))
      (intern (downcase value))
    default))

(defun eplot--vyl (type data &optional default)
  (if-let ((value (cdr (assq type data))))
      (mapcar #'intern (split-string (downcase value)))
    default))

(defun eplot--render (data &optional return-image)
  (let* ((factor (image-compute-scaling-factor))
	 (width (eplot--vn 'width data
			   (or (car eplot-default-size)
			       (/ (* (window-pixel-width
 				      (get-buffer-window "*eplot*" t))
				     0.9)
				  factor))))
	 (height (eplot--vn 'height data
			    (or (cadr eplot-default-size)
				(/ (* (window-pixel-height
				       (get-buffer-window "*eplot*" t))
				      0.9)
				   factor))))
	 (format (eplot--vy 'format data 'normal))
	 (layout (eplot--vy 'layout data 'normal))
	 (dark (eq (eplot--vy 'mode data) 'dark))
	 (compact (eq layout 'compact))
	 (bar-chart (eq format 'bar-chart))
	 (margin-left (eplot--vn 'margin-left data (if compact 30 70)))
	 (margin-right (eplot--vn 'margin-right data (if compact 10 20)))
	 (margin-top (eplot--vn 'margin-top data (if compact 20 40)))
	 (margin-bottom (eplot--vn 'margin-bottom data (if compact 21 60)))
	 (svg (svg-create width height))
	 (font (eplot--vs 'font data "futural"))
	 (font-size (eplot--vn 'font data (if compact 12 14)))
	 (xs (- width margin-left margin-right))
	 (ys (- height margin-top margin-bottom))
	 (chart-color (eplot--vs 'chart-color data (if dark "#c0c0c0" "black")))
	 (axes-color (eplot--vs 'axes-color data chart-color))
	 (grid-color (eplot--vs 'grid-color data
				(if (and dark (not bar-chart))
				    "#404040"
				  "#e0e0e0")))
	 (grid-position (eplot--vy 'grid-position data
				   (if bar-chart 'top 'bottom)))
	 (grid (eplot--vy 'grid data (if bar-chart 'y 'on)))
	 (grid-opacity (eplot--vn 'grid-opacity data
				  (if bar-chart 0.2)))
	 (legend-color (eplot--vs 'legend-color data axes-color))
	 (label-color (eplot--vs 'label-color data
				 (if dark "white" legend-color)))
	 (background-color (eplot--vs 'background-color data
				      (if dark "#101010" "white")))
	 ;; Default bar charts to always start at zero.
	 (min (eplot--vn 'min data (and bar-chart 0)))
	 (max (eplot--vn 'max data))
	 x-type x-values x-ticks stride
	 x-min x-max
	 (possibly-adjust-min t)
	 print-format)
    ;; Add background.
    (svg-rectangle svg 0 0 width height
		   :fill background-color)
    (when-let ((surround-color (eplot--vs 'surround-color data)))
      (svg-rectangle svg 0 0 width height
		     :fill surround-color)
      (svg-rectangle svg margin-left margin-top
		     xs ys
		     :fill (eplot--vs 'background-color data "white")))
    (let ((border-width (eplot--vn 'border-width data))
	  (border-color (eplot--vn 'border-color data)))
      (when (or border-width border-color)
	(svg-rectangle svg 0 0 width height
		       :stroke-width (or border-width 1)
		       :fill "none"
		       :stroke-color (or border-color chart-color))))
    (when-let ((frame-color (eplot--vs 'frame-color data)))
      (svg-rectangle svg margin-left margin-top xs ys
		     :stroke-width (eplot--vn 'frame-width data 1)
		     :fill "none"
		     :stroke-color frame-color))
    ;; Title and legends.
    (when-let ((title (eplot--vs 'title data)))
      (svg-text svg title
		:font-family font
		:text-anchor "middle"
		:font-weight "bold"
		:font-size font-size
		:fill (eplot--vs 'title-color data label-color)
		:x (+ margin-left (/ (- width margin-left margin-right) 2))
		:y (+ 3 (/ margin-top 2))))
    (when-let ((label (eplot--vs 'x-label data)))
      (svg-text svg label
		:font-family font
		:text-anchor "middle"
		:font-weight "bold"
		:font-size font-size
		:fill (eplot--vs 'label-color data label-color)
		:x (+ margin-left (/ (- width margin-left margin-right) 2))
		:y (- height (/ margin-bottom 4))))
    (when-let ((label (eplot--vs 'y-label data)))
      (svg-text svg label
		:font-family font
		:text-anchor "middle"
		:font-weight "bold"
		:font-size font-size
		:fill (eplot--vs 'label-color data label-color)
		:transform
		(format "translate(%s,%s) rotate(-90)"
			(- (/ margin-left 2) (/ font-size 2))
			(+ margin-top
			   (/ (- height margin-bottom margin-top) 2)))))
    ;; Set min/max based on all plots.
    (let ((set-min min)
	  (set-max max))
      (dolist (plot (cdr (assq :plots data)))
	(let* ((values (cdr (assq :values plot)))
	       (data-format (eplot--vyl 'data-format
					(cdr (assq :headers plot))))
	       (vals (nconc (seq-map (lambda (v) (plist-get v :value)) values)
			    (and (memq 'two-values data-format)
				 (seq-map
				  (lambda (v) (plist-get v :extra-value))
				  values)))))
	  ;; Set the x-values based on the first plot.
	  (unless x-values
	    (setq print-format (cond
				((memq 'date data-format) 'date)
				((memq 'time data-format) 'date)
				(t 'number)))
	    (cond
	     ((memq 'xy data-format)
	      (setq x-values (cl-loop for val in values
				      collect (plist-get val :x))
		    x-min (seq-min x-values)
		    x-max (seq-max x-values)
		    x-ticks (eplot--get-ticks x-min x-max xs)
		    stride (e/ xs (- x-max x-min))))
	     ((memq 'date data-format)
	      (setq x-values
		    (cl-loop for val in values
			     collect
			     (time-convert
			      (encode-time
			       (decoded-time-set-defaults
				(iso8601-parse-date
				 (format "%d" (plist-get val :x)))))
			      'integer))
		    x-min (seq-min x-values)
		    x-max (seq-max x-values)
		    x-ticks (eplot--get-ticks x-min x-max xs)
		    stride (e/ xs (- x-max x-min))))
	     (t
	      ;; This is a one-dimensional plot -- we don't have X
	      ;; values, really, so we just do zero to (1- (length
	      ;; values)).
	      (setq x-type 'one-dimensional
		    stride (e/ xs
			       ;; Fenceposting bar-chart vs everything else.
			       (if bar-chart
				   (length values)
				 (1- (length values))))
		    x-values (cl-loop for i from 0
				      repeat (length values)
				      collect i)
		    x-min (car x-values)
		    x-max (car (last x-values))
		    x-ticks x-values))))
	  (unless set-min
	    (setq min (min (or min 1.0e+INF) (seq-min vals)))
	    (unless (= min (car vals))
	      (setq possibly-adjust-min nil)))
	  (unless set-max
	    (setq max (max (or max -1.0e+INF) (seq-max vals)))))))
    ;; Analyze values.
    (let* ((values (cdr (assq :values (car (cdr (assq :plots data))))))
	   (y-ticks (and max
			 (eplot--get-ticks
			  min
			  ;; We get 2% more ticks to check whether we
			  ;; should extend max.
			  (if (eplot--vn 'max data) max (* max 1.02))
			  ys)))
	   x-tick-step x-label-step
	   y-tick-step y-label-step)

      (if bar-chart
	  (setq x-tick-step 1
		x-label-step 1)
	(let ((xt (eplot--compute-x-ticks xs x-values font-size print-format)))
	  (setq x-tick-step (car xt)
		x-label-step (cadr xt))))
      (when max
	(let ((yt (eplot--compute-y-ticks ys y-ticks font-size)))
	  (setq y-tick-step (car yt)
		y-label-step (cadr yt))))
      ;; If max is less than 2% off from a pleasant number, then
      ;; increase max.
      (unless (eplot--vn 'max data)
	(cl-loop for tick in (reverse y-ticks)
		 when (and (< max tick)
			   (< (e/ (- tick max) (- max min)) 0.02))
		 return (progn
			  (setq max tick)
			  ;; Chop off any further ticks.
			  (setcdr (member tick y-ticks) nil))))

      (when y-ticks
	(if (and (not (eplot--vn 'min data))
		 (< (car y-ticks) min))
	    (setq min (car y-ticks))
	  ;; We may be extending the bottom of the chart to get pleasing
	  ;; numbers.  We don't want to be drawing the chart on top of the
	  ;; X axis, because the chart won't be visible there.
	  (when (and (<= min (car y-ticks))
		     possibly-adjust-min
		     ;; But not if we start at origo, because that just
		     ;; looks confusing.
		     (not (zerop min)))
	    (setq min (- (car y-ticks)
			 ;; 2% of the value range.
			 (* 0.02 (- (car (last y-ticks)) (car y-ticks))))))))

      (when (eq grid-position 'top)
	(eplot--draw-plots data chart-color height margin-bottom margin-left
			   min max xs ys stride svg margin-top
			   x-values x-min x-max))
      ;; Make X ticks.
      (cl-loop for x in x-ticks
	       for i from 0
	       for label = (if bar-chart
			       (eplot--vs 'label
					  (plist-get (elt values x) :settings)
					  (format "%s" x))
			     (eplot--format-value x print-format))
	       for px = (if bar-chart
			    (+ margin-left (* x stride) (/ stride 2)
			       (/ (* stride 0.1) 2))
			  (+ margin-left
			     (* (/ (- (* 1.0 x) x-min) (- x-max x-min))
				xs)))
	       ;; We might have one extra stride outside the area -- don't
	       ;; draw it.
	       when (<= px (- width margin-right))
	       do
	       (when (zerop (e% x x-tick-step))
		 ;; Draw little tick.
		 (unless bar-chart
		   (svg-line svg
			     px (- height margin-bottom)
			     px (+ (- height margin-bottom)
				   (if (zerop (e% x x-label-step))
				       4
				     2))
			     :stroke legend-color))
		 (when (or (eq grid 'on) (eq grid 'x))
		   (svg-line svg px margin-top
			     px (- height margin-bottom)
			     :opacity grid-opacity
			     :stroke grid-color)))
	       (when (and (zerop (e% x x-label-step))
			  ;; We want to skip marking the first X value
			  ;; unless we're a bar chart or we're a one
			  ;; dimensional chart.
			  (or bar-chart
			      (not (= x-min (car x-values)))
			      (eq x-type 'one-dimensional)
			      (and (not (zerop x)) (not (zerop i)))))
		 (svg-text svg label
			   :font-family font
			   :text-anchor "middle"
			   :font-size font-size
			   :fill legend-color
			   :x px
			   :y (+ (- height margin-bottom)
				 font-size
				 (if bar-chart
				     (if compact 3 5)
				   2)))))
      ;; Make Y ticks.
      (cl-loop for y in y-ticks
	       for i from 0
	       for py = (- (- height margin-bottom)
			   (* (/ (- (* 1.0 y) min) (- max min))
			      ys))
	       do
	       (when (and (<= margin-top py (- height margin-bottom))
			  (zerop (e% y y-tick-step)))
		 (svg-line svg margin-left py
			   (- margin-left 3) py
			   :stroke-color axes-color)
		 (when (or (eq grid 'on) (eq grid 'y))
		   (svg-line svg margin-left py
			     (- width margin-right) py
			     :opacity grid-opacity
			     :stroke-color grid-color))
		 (when (zerop (e% y y-label-step))
		   (svg-text svg (eplot--format-y
				  y (- (cadr y-ticks) (car y-ticks)) nil)
			     :font-family font
			     :text-anchor "end"
			     :font-size font-size
			     :fill legend-color
			     :x (- margin-left 6)
			     :y (+ py (/ font-size 2) -2)))))
      
      ;; Draw axes.
      (svg-line svg margin-left margin-top margin-left
		(+ (- height margin-bottom) 5)
		:stroke axes-color)
      (svg-line svg (- margin-left 5) (- height margin-bottom)
		(- width margin-right) (- height margin-bottom)
		:stroke axes-color)

      (when (eq grid-position 'bottom)
	(eplot--draw-plots data chart-color height margin-bottom margin-left
			   min max xs ys stride svg margin-top
			   x-values x-min x-max))

      (when-let ((frame-color (eplot--vs 'frame-color data)))
	(svg-rectangle svg margin-left margin-top xs ys
		       :stroke-width (eplot--vn 'frame-width data 1)
		       :fill "none"
		       :stroke-color frame-color))

      (when (eplot--vs 'legend data)
	(when-let ((names
		    (cl-loop for plot in (cdr (assq :plots data))
			     for headers = (cdr (assq :headers plot))
			     for name = (eplot--vs 'name headers)
			     when name
			     collect
			     (cons name (eplot--vs 'legend-color headers)))))
	  (svg-rectangle svg (+ margin-left 20) (+ margin-top 20)
			 (format "%dex"
				 (+ 2
				    (seq-max (mapcar (lambda (name)
						       (length (car name)))
						     names))))
			 (* font-size (+ (length names) 2))
			 :font-size font-size
			 :fill-color (eplot--vs 'legend-background-color data
						background-color)
			 :stroke-color (eplot--vs 'legend-border-color data
						  axes-color))
	  (cl-loop for name in names
		   for i from 0
		   do (svg-text svg (car name)
			       :font-family font
			       :text-anchor "front"
			       :font-size font-size
			       :fill (or (cdr name) legend-color)
			       :x (+ margin-left 25)
			       :y (+ margin-top 40 (* i font-size)))))))

    (if return-image
	svg
      (svg-insert-image svg))))

(defun eplot--format-y (y spacing whole)
  (cond
   ((or (= (round (* spacing 100)) 10) (= (round (* spacing 100)) 20))
    (format "%.1f" y))
   ((< spacing 0.01)
    (format "%.3f" y))
   ((< spacing 1)
    (format "%.2f" y))
   ((and (< spacing 1) (not (zerop (mod (* spacing 10) 1))))
    (format "%.1f" y))
   ((zerop (% spacing 1000000000))
    (format "%dG" (/ y 1000000000)))
   ((zerop (% spacing 1000000))
    (format "%dM" (/ y 1000000)))
   ((zerop (% spacing 1000))
    (format "%dk" (/ y 1000)))
   ((>= spacing 1)
    (format "%s" y))
   ((not whole)
    (format "%.1f" y))
   (t
    (format "%s" y))))

(defun eplot--format-value (value print-format)
  (cond
   ((eq print-format 'date)
    (format-time-string "%Y-%m-%d" value))
   ((eq print-format 'year)
    (format-time-string "%Y" value))
   ((eq print-format 'time)
    (format-time-string "%H:%M:%S" value))
   (t
    (format "%s" value))))

(defun eplot--compute-x-ticks (xs x-values font-size print-format)
  (let* ((min (seq-min x-values))
	 (max (seq-max x-values))
	 (count (length x-values))
	 (max-print (eplot--format-value max print-format))
	 ;; We want each label to be spaced at least as long apart as
	 ;; the length of the longest label, with room for two blanks
	 ;; in between.
	 (min-spacing (* (+ (length max-print) 2) (e/ font-size 2)))
	 (digits (eplot--decimal-digits (- (cadr x-values) (car x-values))))
	 (every (e/ 1 (expt 10 digits))))
    (cond
     ;; We have room for every X value.
     ((< (* count min-spacing) xs)
      (list every every))
     ;; We have to prune X labels, but not grid lines.  (We shouldn't
     ;; have a grid line more than every 10 pixels.)
     ((< (* count 10) xs)
      (list every
	    (let ((label-step every))
	      (while (> (/ (- max min) label-step) (/ xs min-spacing))
		(setq label-step (eplot--next-weed label-step)))
	      label-step)))
     ;; We have to reduce both grid lines and labels.
     (t
      (let ((tick-step every))
	(while (> (/ (- max min) tick-step) (/ xs 10))
	  (setq tick-step (eplot--next-weed tick-step)))
	(list tick-step
	      (let ((label-step tick-step))
		(while (> (/ (- max min) label-step) (/ xs min-spacing))
		  (setq label-step (eplot--next-weed label-step))
		  (while (not (zerop (% label-step tick-step)))
		    (setq label-step (eplot--next-weed label-step))))
		label-step)))))))

(defun eplot--compute-y-ticks (ys y-values font-size)
  (let* (;;(min (car x-values))
	 (count (length y-values))
	 ;; We want each label to be spaced at least as long apart as
	 ;; the length of the longest label, with room for two blanks
	 ;; in between.
	 (min-spacing (* font-size 0.9))
	 (digits (eplot--decimal-digits (- (cadr y-values) (car y-values))))
	 (every (e/ 1 (expt 10 digits))))
    (cond
     ;; We have room for every X value.
     ((< (* count min-spacing) ys)
      (list every every))
     ;; We have to prune Y labels, but not grid lines.  (We shouldn't
     ;; have a grid line more than every 10 pixels.)
     ((< (* count 10) ys)
      (list every
	    (let ((label-step every))
	      (while (> (/ count label-step) (/ ys min-spacing))
		(setq label-step (eplot--next-weed label-step)))
	      label-step)))
     ;; We have to reduce both grid lines and labels.
     (t
      (let ((tick-step 1))
	(while (> (/ count tick-step) (/ ys 10))
	  (setq tick-step (eplot--next-weed tick-step)))
	(list tick-step
	      (let ((label-step tick-step))
		(while (> (/ count label-step) (/ ys min-spacing))
		  (setq label-step (eplot--next-weed label-step))
		  (while (not (zerop (% label-step tick-step)))
		    (setq label-step (eplot--next-weed label-step))))
		label-step)))))))

(defun eplot--next-weed (weed)
  (let (digits series)
    (if (>= weed 1)
	(setq digits (truncate (log weed 10))
	      series (/ weed (expt 10 digits)))
      (setq digits (eplot--decimal-digits weed)
	    series (* weed (expt 10 digits))))
    (cond
     ((= series 1)
      (if (>= weed 1)
	  (* 2 (expt 10 digits))
	(e/ 2 (expt 10 digits))))
     ((= series 2)
      (if (>= weed 1)
	  (* 5 (expt 10 digits))
	(e/ 5 (expt 10 digits))))
     ((= series 5)
      (if (>= weed 1)
	  (* 10 (expt 10 digits))
	(e/ 10 (expt 10 digits))))
     (t
      (error "Invalid weed: %s" weed)))))

(defun eplot--parse-gradient (string)
  (when string
    (let ((bits (split-string string)))
      (list
       (cons 'from (nth 0 bits))
       (cons 'to (nth 1 bits))
       (cons 'direction (intern (or (nth 2 bits) "top-down")))
       (cons 'position (intern (or (nth 3 bits) "below")))))))

(defun eplot--smooth (values algo xs)
  (if (not algo)
      values
    (let* ((vals (cl-coerce values 'vector))
	   (max (1- (length vals)))
	   (period (* 4 (ceiling (/ max xs)))))
      (cl-case algo
	(moving-average
	 (cl-loop for i from 0 upto max
		  collect (e/ (cl-loop for ii from 0 upto (1- period)
				       sum (elt vals (min (+ i ii) max)))
			      period)))))))

(defun eplot--vary-color (color n)
  (let ((colors ["#e6194b" "#3cb44b" "#ffe119" "#4363d8" "#f58231" "#911eb4"
		 "#46f0f0" "#f032e6" "#bcf60c" "#fabebe" "#008080" "#e6beff"
		 "#9a6324" "#fffac8" "#800000" "#aaffc3" "#808000" "#ffd8b1"
		 "#000075" "#808080" "#ffffff" "#000000"]))
    (unless (equal color "vary")
      (setq colors
	    (if (string-search " " color)
		(split-string color)
	      (list color))))
    (elt colors (mod n (length colors)))))

(defun eplot--draw-plots (data default-color height
			       margin-bottom margin-left
			       min max xs ys
			       stride svg margin-top
			       x-values x-min x-max)
  ;; Draw all the plots.
  (cl-loop for plot in (reverse (cdr (assq :plots data)))
	   for plot-number from 0
	   for headers = (cdr (assq :headers plot))
	   for values = (cdr (assq :values plot))
	   for vals = (eplot--smooth
		       (seq-map (lambda (v) (plist-get v :value)) values)
		       (eplot--vy 'smoothing headers)
		       xs)
	   for polygon = nil
	   for gradient = (eplot--parse-gradient (eplot--vs 'gradient headers))
	   for lpy = nil
	   for lpx = nil
	   for style = (if (eq (eplot--vy 'format data) 'bar-chart)
			   'bar
			 (eplot--vy 'style headers 'line))
	   for bar-gap = (* stride 0.1)
	   do
	   (unless gradient
	     (when-let ((fill (eplot--vs 'fill headers)))
	       (setq gradient `((from . ,fill) (to . ,fill)
				(direction . top-down) (position . below)))))
	   (when gradient
	     (if (eq (eplot--vs 'position gradient) 'above)
		 (push (cons margin-left margin-top) polygon)
	       (push (cons margin-left (- height margin-bottom)) polygon)))
	   (cl-loop
	    for val in vals
	    for value in values
	    for x in x-values
	    for i from 0
	    for settings = (plist-get value :settings)
	    for color = (eplot--vary-color
			 (eplot--vs 'color settings
				    (eplot--vs 'color headers default-color))
			 i)
	    for py = (- (- height margin-bottom)
			(* (/ (- (* 1.0 val) min) (- max min))
			   ys))
	    for px = (if (eq style 'bar)
			 (+ margin-left
			    (* (e/ (- x x-min) (- x-max x-min -1))
			       xs))
		       (+ margin-left
			  (* (e/ (- x x-min) (- x-max x-min))
			     xs)))
	    do
	    (cl-case style
	      (bar
	       (svg-rectangle svg
			      (+ px bar-gap) py
			      (- stride bar-gap) (- height margin-bottom py)
			      :fill color))
	      (impulse
	       (svg-line svg
			 px py
			 px (- height margin-bottom)
			 :stroke color))
	      (point
	       (svg-line svg px py (1+ px) (1+ py)
			 :stroke color))
	      (line
	       ;; If we're doing a gradient, we're just collecting
	       ;; points and will draw the polygon later.
	       (if gradient
		   (push (cons px py) polygon)
		 (when lpx
		   (svg-line svg lpx lpy px py
			     :stroke color))))
	      (square
	       (if gradient
		   (progn
		     (when lpx
		       (push (cons lpx py) polygon))
		     (push (cons px py) polygon))
		 (when lpx
		   (svg-line svg lpx lpy px lpy
			     :stroke color)
		   (svg-line svg px lpy px py
			     :stroke color))))
	      (circle
	       (svg-circle svg px py
			   (eplot--vn 'size settings
				      (eplot--vn 'size headers 3))
			   :stroke color
			   :fill (eplot--vary-color
				  (eplot--vs 'fill settings
					     (eplot--vs 'fill headers "none"))
				  i)))
	      (cross
	       (let ((s (eplot--vn 'size headers 3)))
		 (svg-line svg (- px s) (- py s)
			   (+ px s) (+ py s)
			   :stroke color)
		 (svg-line svg (+ px s) (- py s)
			   (- px s) (+ py s)
			   :stroke color)))
	      (triangle
	       (let ((s (eplot--vn 'size headers 5)))
		 (svg-polygon svg
			      (list
			       (cons (- px (e/ s 2)) (+ py (e/ s 2)))
			       (cons px (- py (e/ s 2)))
			       (cons (+ px (e/ s 2)) (+ py (e/ s 2))))
			      :stroke color
			      :fill-color (eplot--vs 'fill headers "none"))))
	      (rectangle
	       (let ((s (eplot--vn 'size headers 3)))
		 (svg-rectangle svg (- px (e/ s 2)) (- py (e/ s 2))
				s s
				:stroke color
				:fill-color (eplot--vs 'fill headers "none")))))
	    (setq lpy py
		  lpx px))

	   ;; We're doing a gradient of some kind, so draw it now when
	   ;; we've collected the polygon.
	   (when polygon
	     ;; We have a "between" chart, so collect the data points
	     ;; from the "extra" values, too.
	     (when (memq 'two-values (eplot--vyl 'data-format headers))
	       (cl-loop
		for val in (nreverse
			    (seq-map (lambda (v) (plist-get v :extra-value))
				     values))
		for x from (1- (length vals)) downto 0
		for py = (- (- height margin-bottom)
			    (* (/ (- (* 1.0 val) min) (- max min))
			       ys))
		for px = (+ margin-left
			    (* (e/ (- x x-min) (- x-max x-min))
			       xs))
		do
		(cl-case style
		  (line
		   (push (cons px py) polygon))
		  (square
		   (when lpx
		     (push (cons lpx py) polygon))
		   (push (cons px py) polygon)))
		(setq lpx px lpy py)))

	     (if (eq (eplot--vs 'position gradient) 'above)
		 (push (cons lpx margin-top) polygon)
	       (push (cons lpx (- height margin-bottom)) polygon))
	     (let ((id (format "gradient-%d" plot-number)))
	       (eplot--gradient svg id 'linear
				(eplot--stops (eplot--vs 'from gradient)
					      (eplot--vs 'to gradient))
				(eplot--vs 'direction gradient))
	       (svg-polygon svg (nreverse polygon)
			    :gradient id
			    :stroke (eplot--vs 'fill-border headers))
	       (setq polygon nil)))))

(defun eplot--stops (from to)
  (append `((0 . ,from))
	  (cl-loop for (pct col) on (split-string to "-") by #'cddr
		   collect (if col
			       (cons (string-to-number pct) col)
			     (cons 100 pct)))))

(defun eplot--gradient (svg id type stops &optional direction)
  "Add a gradient with ID to SVG.
TYPE is `linear' or `radial'.

STOPS is a list of percentage/color pairs.

DIRECTION is one of `top-down', `bottom-up', `left-right' or `right-left'.
nil means `top-down'."
  (svg--def
   svg
   (apply
    #'dom-node
    (if (eq type 'linear)
	'linearGradient
      'radialGradient)
    `((id . ,id)
      (x1 . ,(if (eq direction 'left-right) 1 0))
      (x2 . ,(if (eq direction 'right-left) 1 0))
      (y1 . ,(if (eq direction 'bottom-up) 1 0))
      (y2 . ,(if (eq direction 'top-down) 1 0)))
    (mapcar
     (lambda (stop)
       (dom-node 'stop `((offset . ,(format "%s%%" (car stop)))
			 (stop-color . ,(cdr stop)))))
     stops))))

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
	 (even (eplot--pleasing-numbers (* (e/ diff height) 10)))
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

(defun eplot--pleasing-numbers (number)
  (let* ((digits (eplot--decimal-digits number))
	 (one (e/ 1 (expt 10 digits)))
	 (two (e/ 2 (expt 10 digits)))
	 (five (e/ 5 (expt 10 digits))))
    (catch 'found
      (while t
	(when (< number one)
	  (throw 'found one))
	(setq one (* one 10))
	(when (< number two)
	  (throw 'found two))
	(setq two (* two 10))
	(when (< number five)
	  (throw 'found five))
	(setq five (* five 10))))))

(defun eplot-test-plots (&optional main)
  (interactive "P")
  (save-current-buffer
    (let ((spacer (svg-create 1 1))
	  (width 0))
      (svg-rectangle spacer 0 0 1 1 :fill "black")
      (if (get-buffer-window "*test eplots*" t)
	  (set-buffer "*test eplots*")
	(pop-to-buffer "*test eplots*")
	(when (< text-scale-mode-amount (abs (text-scale-min-amount)))
	  (text-scale-decrease (abs (text-scale-min-amount)))))
      (erase-buffer)
      (cl-loop for file in (directory-files "examples" t
					    (if main "^chart.*.plt\\'"
					      "plt\\'"))
	       for i from 0
	       do (let ((image-scaling-factor 1.2)
			(start (point)))
		    (eplot-parse-and-insert file)
		    ;; So that you can hover over a chart and see its
		    ;; file name.
		    (put-text-property
		     start (point) 'help-echo (file-name-nondirectory file))
		    (let ((w (car (image-size
				   (get-text-property start 'display) t))))
		      (if (not (> (+ w width) (+ 80 (window-pixel-width))))
			  (cl-incf width w)
			(goto-char start)
			(insert "\n\n")
			(goto-char (point-max))
			(setq width w))))
	       (let ((start (point)))
		 (svg-insert-image spacer)
		 (put-text-property start (point) 'spacer t)))
      (goto-char (point-min))
      (eplot--fold-buffer)
      (insert "\n\n")
      (goto-char (point-min)))))

(defun eplot-parse-and-insert (file)
  "Parse and insert a file in the current buffer."
  (interactive "fEplot file: ")
  (let ((default-directory (file-name-directory file)))
    (eplot--render (with-temp-buffer
		     (insert-file-contents file)
		     (eplot--parse-buffer)))))

(defun eplot-write-plots (&optional all)
  (interactive "P")
  (cl-loop for file in (directory-files "examples" t "[.]plt\\'")
	   for image = (let ((default-directory (file-name-directory file)))
			 (eplot--render (with-temp-buffer
					  (insert-file-contents file)
					  (eplot--parse-buffer))
					t))
	   for svg = (expand-file-name (file-name-with-extension
					(file-name-nondirectory file)
					".svg")
				       "images")
	   for png = (file-name-with-extension svg ".png")
	   when (or all (file-newer-than-file-p file png))
	   do (with-temp-buffer
		(set-buffer-multibyte nil)
		(svg-print image)
		(write-region (point-min) (point-max) svg)
		(call-process "convert" nil nil nil svg png)
		(delete-file svg))))

(provide 'eplot)

;;; eplot.el ends here

;;; Todo:

;; Date plot
;; Time plot
