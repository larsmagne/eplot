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

(defun eplot--parse-values (&optional in-headers)
  ;; Skip past separator lines.
  (while (looking-at "[ \t]*\n")
    (forward-line 1))
  (let* ((values nil)
	 ;; We may have plot-specific headers.
	 (headers (eplot--parse-headers))
	 (two-values (or (eplot--vs 'data-format headers)
			 (eplot--vs 'data-format in-headers)))
	 extra-value)
    (if-let ((data-file (eplot--vs 'data headers)))
	(with-temp-buffer
	  (insert-file-contents data-file)
	  (setq values (cdr (assq :values (eplot--parse-values headers)))
		headers (delq (assq 'data headers) headers)))
      ;; Now we come to the data.  The data is typically either just a
      ;; number, or two numbers (in which case the first number is a
      ;; date or a time).  Labels can be introduced with a # char.
      (while (looking-at
	      "^[ \t]*\\([0-9.]+\\)\\([ \t]+\\([0-9.]+\\)\\)?\\([ \t]+\\([0-9.]+\\)\\)?\\([ \t]+#\\(.*\\)\\)?")
	(let ((v1 (string-to-number (match-string 1)))
	      (v2 (match-string 3))
	      (v3 (match-string 5))
	      (label (match-string 7))
	      this)
	  (when (and two-values (not v3))
	    (setq extra-value (string-to-number v2)
		  v2 nil))
	  (setq this
		(cond
		 ((and v2 label)
		  (list :value (string-to-number v2) :x v1
			:label (substring-no-properties label)))
		 (v2
		  (list :value (string-to-number v2) :x v1))
		 (label
		  (list :value v1 :label (substring-no-properties label)))
		 (t
		  (list :value v1))))
	  (when extra-value
	    (setq this (nconc this (list :extra-value extra-value))))
	  (push this values))
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
	 (format (eplot--vy 'format data 'normal))
	 (compact (eq format 'compact))
	 (margin-left (eplot--vn 'margin-left data (if compact 30 70)))
	 (margin-right (eplot--vn 'margin-right data (if compact 10 20)))
	 (margin-top (eplot--vn 'margin-top data (if compact 20 40)))
	 (margin-bottom (eplot--vn 'margin-bottom data (if compact 21 60)))
	 (svg (svg-create width height))
	 (font (eplot--vs 'font data "futural"))
	 (font-size (eplot--vn 'font data (if compact 12 14)))
	 (xs (- width margin-left margin-right))
	 (ys (- height margin-top margin-bottom))
	 (color (eplot--vs 'color data "black"))
	 (axes-color (eplot--vs 'axes-color data color))
	 (grid-color (eplot--vs 'grid-color data "#e0e0e0"))
	 (grid-position (eplot--vy 'grid-position data 'bottom))
	 (grid (eplot--vy 'grid data 'on))
	 (legend-color (eplot--vs 'legend-color data axes-color))
	 (background-color (eplot--vs 'background-color data "white"))
	 ;; Default bar charts to always start at zero.
	 (min (eplot--vn 'min data (and (eq format 'bar-chart) 0)))
	 (max (eplot--vn 'max data)))
    ;; Add background.
    (svg-rectangle svg 0 0 width height
		   :fill background-color)
    (when-let ((surround-color (eplot--vs 'surround-color data)))
      (svg-rectangle svg 0 0 width height
		     :fill surround-color)
      (svg-rectangle svg margin-left margin-top
		     xs ys
		     :fill (eplot--vs 'background-color data "white")))
    (when-let ((border-color (eplot--vs 'border-color data)))
      (svg-rectangle svg 0 0 width height
		     :stroke-width (eplot--vn 'border-width data 1)
		     :fill "none"
		     :stroke-color border-color))
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
		:fill (eplot--vs 'title-color data legend-color)
		:x (+ margin-left (/ (- width margin-left margin-right) 2))
		:y (+ 3 (/ margin-top 2))))
    (when-let ((label (eplot--vs 'x-label data)))
      (svg-text svg label
		:font-family font
		:text-anchor "middle"
		:font-weight "bold"
		:font-size font-size
		:fill legend-color
		:x (+ margin-left (/ (- width margin-left margin-right) 2))
		:y (- height (/ margin-bottom 4))))
    (when-let ((label (eplot--vs 'y-label data)))
      (svg-text svg label
		:font-family font
		:text-anchor "middle"
		:font-weight "bold"
		:font-size font-size
		:fill legend-color
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
	       (vals (nconc (seq-map (lambda (v) (plist-get v :value)) values)
			    (and (equal (eplot--vs 'data-format
						   (cdr (assq :headers plot)))
					"two-values")
				 (seq-map
				  (lambda (v) (plist-get v :extra-value))
				  values)))))
	  (unless set-min
	    (setq min (min (or min 1.0e+INF) (seq-min vals))))
	  (unless set-max
	    (setq max (max (or max -1.0e+INF) (seq-max vals)))))))
    ;; Analyze values.
    (let* ((values (cdr (assq :values (car (cdr (assq :plots data))))))
	   (stride (e/ xs
		       ;; Fenceposting bar-chart vs everything else.
		       (if (eq format 'bar-chart)
			   (length values)
			 (1- (length values)))))
	   (x-ticks (eplot--get-ticks 0 (length values) xs
				      (eq format 'bar-chart)))
	   (y-ticks (eplot--get-ticks
		     min
		     ;; We get 2% more ticks to check whether we
		     ;; should extend max.
		     (if (eplot--vn 'max data) max (* max 1.02))
		     ys))
	   ;; This is how often we should output labels on the ticks.
	   (step (if (eq format 'bar-chart)
		     1
		   (ceiling (e/ (length x-ticks) (e/ width 70))))))

      ;; If max is less than 2% off from a pleasant number, then
      ;; increase max.
      (unless (eplot--vn 'max data)
	(cl-loop for tick in (reverse y-ticks)
		 when (and (< max tick)
			   (< (e/ (- tick max) max) 0.02))
		 return (progn
			  (setq max tick)
			  ;; Chop off any further ticks.
			  (setcdr (member tick y-ticks) nil))))

      ;; We may be extending the bottom of the chart to get pleasing
      ;; numbers.  We don't want to be drawing the chart on top of the
      ;; X axis, because the chart won't be visible there.
      (when (<= min (car y-ticks))
	(setq min (- (car y-ticks)
		     ;; 2% of the value range.
		     (* 0.02 (- (car (last y-ticks)) (car y-ticks))))))
      
      (when (eq grid-position 'top)
	(eplot--draw-plots data color height margin-bottom margin-left
			   min max xs ys stride svg margin-top))

      ;; Make X ticks.
      (cl-loop for x in x-ticks
	       for label = (if (eq format 'bar-chart)
			       (plist-get (elt values x) :label)
			     (format "%s" x))
	       for px = (if (eq format 'bar-chart)
			    (+ margin-left (* x stride) (/ stride 2))
			  (+ margin-left (* x stride)))
	       ;; We might have one extra stride outside the area -- don't
	       ;; draw it.
	       when (> px (- width margin-right))
	       return nil
	       do
	       ;; Draw little tick.
	       (unless (eq format 'bar-chart)
		 (svg-line svg
			   px (- height margin-bottom)
			   px (+ (- height margin-bottom)
				 (if (zerop (e% x step))
				     4
				   2))
			   :stroke legend-color))
	       (when (or (eq grid 'on) (eq grid 'x))
		 (svg-line svg px margin-top
			   px (- height margin-bottom)
			   :stroke grid-color))
	       when (zerop (e% x step))
	       do (svg-text svg label
			    :font-family font
			    :text-anchor "middle"
			    :font-size font-size
			    :fill legend-color
			    :x px
			    :y (+ (- height margin-bottom)
				  font-size
				  (if (eq format 'bar-chart)
				      5
				    2))))
      ;; Make Y ticks.
      (let* ((ideal (1+ (ceiling (e/ ys font-size))))
	     factor)
	(setq factor (eplot--pleasing-numbers
		      (ceiling (e/ ys ideal))))
	;; If we get a too big factor here, we decrease it.
	(when (< (e/ (- max min) factor) 2)
	  (setq factor (eplot--pleasing-numbers
			(ceiling (e/ (length y-ticks) ideal 2)))))
	(cl-loop for y in y-ticks
		 for i from 0
		 for py = (- (- height margin-bottom)
			     (* (/ (- (* 1.0 y) min) (- max min))
				ys))
		 do
		 (when (< py (- height margin-bottom))
		   (svg-line svg margin-left py
			     (- margin-left 3) py
			     :stroke-color axes-color)
		   (when (or (eq grid 'on) (eq grid 'y))
		     (svg-line svg margin-left py
			       (- width margin-right) py
			       :stroke-color grid-color))
		   (when (zerop (e% y factor))
		     (svg-text svg (eplot--format-y
				    y (- (elt y-ticks 1) (elt y-ticks 0))
				    nil)
			       :font-family font
			       :text-anchor "end"
			       :font-size font-size
			       :fill legend-color
			       :x (- margin-left 6)
			       :y (+ py (/ font-size 2) -2))))))
      
      ;; Draw axes.
      (svg-line svg margin-left margin-top margin-left
		(+ (- height margin-bottom) 5)
		:stroke axes-color)
      (svg-line svg (- margin-left 5) (- height margin-bottom)
		(- width margin-right) (- height margin-bottom)
		:stroke axes-color)

      (when (eq grid-position 'bottom)
	(eplot--draw-plots data color height margin-bottom margin-left
			   min max xs ys stride svg margin-top))

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
    
    (svg-insert-image svg)))

(defun eplot--format-y (y spacing whole)
  (cond
   ((or (= (ceiling (* spacing 100)) 10) (= (ceiling (* spacing 100)) 20))
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
  (let ((colors '("red" "green" "blue" "cyan" "yellow" "purple")))
    (if (not (equal color "vary"))
	color
      (elt colors (mod n (length colors))))))

(defun eplot--draw-plots (data default-color height
			       margin-bottom margin-left
			       min max xs ys
			       stride svg margin-top)
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
	    for x from 0
	    for color = (eplot--vary-color
			 (eplot--vs 'color headers default-color)
			 x)
	    for py = (- (- height margin-bottom)
			(* (/ (- (* 1.0 val) min) (- max min))
			   ys))
	    for px = (+ margin-left (* x stride))
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
	       (svg-circle svg px py (eplot--vn 'size headers 3)
			   :stroke color))
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
	     (when (equal (eplot--vs 'data-format headers) "two-values")
	       (cl-loop
		for val in (nreverse
			    (seq-map (lambda (v) (plist-get v :extra-value))
				     values))
		for x from (1- (length vals)) downto 0
		for py = (- (- height margin-bottom)
			    (* (/ (- (* 1.0 val) min) (- max min))
			       ys))
		for px = (+ margin-left (* x stride))
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
				`((0 . ,(eplot--vs 'from gradient))
				  (100 . ,(eplot--vs 'to gradient)))
				(eplot--vs 'direction gradient))
	       (svg-polygon svg (nreverse polygon)
			    :gradient id
			    :stroke (eplot--vs 'fill-border headers))
	       (setq polygon nil)))))

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
	 (one (if (zerop digits) 1 (/ 1.0 (expt 10 digits))))
	 (two (if (zerop digits) 2 (/ 2.0 (expt 10 digits))))
	 (five (if (zerop digits) 5 (/ 5.0 (expt 10 digits)))))
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

(defun eplot-test-plots ()
  (interactive)
  (save-current-buffer
    (if (get-buffer-window "*test eplots*" t)
	(set-buffer "*test eplots*")
      (pop-to-buffer "*test eplots*"))
    (erase-buffer)
    (cl-loop for file in (directory-files "examples" t "chart.*.txt\\'")
	     for i from 0
	     when (and (cl-plusp i)
		       (zerop (% i 3)))
	     do (insert "\n\n")
	     do (let ((image-scaling-factor 1.57))
		  (eplot-parse-and-insert file))
	     (insert " "))))

(defun eplot-parse-and-insert (file)
  "Parse and insert a file in the current buffer."
  (interactive "fEplot file: ")
  (let ((default-directory (file-name-directory file)))
    (eplot--render (with-temp-buffer
		     (insert-file-contents file)
		     (eplot--parse-buffer)))))

(provide 'eplot)

;;; eplot.el ends here

;;; Todo:
;; Per-data circle size...
;; Choose which column of data to use
