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
;;
;; If installing manually, put something like the following in your
;; Emacs init file (but adjust the path to where you've put eplot):
;; 
;; (push "~/src/eplot/" load-path)
;; (autoload 'eplot "eplot" nil t)
;; (autoload 'eplot-mode "eplot" nil t)
;; (unless (assoc "\\.plt" auto-mode-alist)
;;   (setq auto-mode-alist (cons '("\\.plt" . eplot-mode) auto-mode-alist)))

;;; Code:

(require 'svg)
(require 'cl-lib)
(require 'face-remap)
(require 'eieio)
(require 'iso8601)
(require 'transient)

(defvar eplot--user-defaults (make-hash-table))

(defun eplot-set (header value)
  "Set the default value of HEADER to VALUE.
To get a list of all possible HEADERs, use the `M-x
eplot-list-chart-headers' command.

Also see `eplot-reset'."
  (let ((elem (or (assq header eplot--chart-headers)
		  (assq header eplot--plot-headers))))
    (unless elem
      (error "No such header type: %s" header))
    (setf (gethash header eplot--user-defaults) value)))

(defun eplot-reset (&optional header)
  "Reset HEADER to defaults.
If HEADER is nil or not present, reset everything to defaults."
  (if header
      (remhash header eplot--user-defaults)
    (setq eplot--user-defaults (make-hash-table))))

(unless (assoc "\\.plt" auto-mode-alist)
  (setq auto-mode-alist (cons '("\\.plt" . eplot-mode) auto-mode-alist)))

;;; eplot modes.

(defvar-keymap eplot-mode-map
  "C-c C-c" #'eplot-update-view-buffer
  "C-c C-e" #'eplot-list-chart-headers
  "TAB" #'eplot-complete)

;; # is working overtime in the syntax here:
;;  It can be a color like Color: #e0e0e0, and
;;  it can be a setting like 33 # Label: Apples,
;;  when it starts a line it's a comment.
(defvar eplot-font-lock-keywords
  `(("^[ \t\n]*#.*" . font-lock-comment-face)
    ("^[^ :\n]+:" . font-lock-keyword-face)
    ("#[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]\\([0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]\\)?" . font-lock-variable-name-face)
    ("#.*" . font-lock-builtin-face)))

(define-derived-mode eplot-mode text-mode "eplot"
  "Major mode for editing charts.
Use the \\[eplot-list-chart-headers] command to get a list of all
possible chart headers."
  (setq-local completion-at-point-functions
	      (cons 'eplot--complete-header completion-at-point-functions))
  (setq-local font-lock-defaults
	      '(eplot-font-lock-keywords nil nil nil)))

(defun eplot-complete ()
  "Complete headers."
  (interactive)
  (cond
   ((let ((completion-fail-discreetly t))
      (completion-at-point))
    ;; Completion was performed; nothing else to do.
    nil)
   (t (indent-relative))))

(defun eplot--complete-header ()
  (and (save-excursion
	 (or (looking-at ".*:")
	     (looking-at "[ \t]*$")))
       (lambda ()
	 (let ((headers (mapcar
			 (lambda (h)
			   (capitalize (symbol-name (car h))))
			 (save-excursion
			   ;; If we're after the headers, then we want
			   ;; to complete over the plot headers.  Otherwise,
			   ;; complete over the chart headers.
			   (if (and (not (bobp))
				    (progn
				      (forward-line -1)
				      (re-search-backward "^[ \t]*$" nil t)))
			       eplot--plot-headers
			     eplot--chart-headers))))
	       (completion-ignore-case t))
	   (completion-in-region (pos-bol) (line-end-position) headers)
	   'completion-attempted))))

(define-minor-mode eplot-minor-mode
  "Minor mode to issue commands from an eplot data buffer."
  :lighter " eplot")

(defvar-keymap eplot-minor-mode-map
  "H-l" #'eplot-eval-and-update)

(defvar-keymap eplot-view-mode-map
  "C-c C-s" #'eplot-view-write-file
  "C-c C-w" #'eplot-view-write-scaled-file)

(define-derived-mode eplot-view-mode special-mode "eplot view"
  "Major mode for displaying eplots."
  (setq-local revert-buffer-function #'eplot-update))

(defun eplot-view-write-file (file &optional width)
  "Write the current chart to a file.
If you type in a file name that ends with something else than \"svg\",
ImageMagick \"convert\" will be used to convert the image first.

If writing to a PNG file, \"rsvg-conver\" will be used instead if
it exists as this usually gives better results."
  (interactive "FWrite to file name: ")
  (when (and (file-exists-p file)
	     (not (yes-or-no-p "File exists, overwrite? ")))
    (error "Not overwriting the file"))
  (save-excursion
    (goto-char (point-min))
    (let ((match
	   (text-property-search-forward 'display nil
					 (lambda (_ e)
					   (and (consp e)
						(eq (car e) 'image))))))
      (unless match
	(error "Can't find an image in the current buffer"))
      (let ((svg (plist-get (cdr (prop-match-value match)) :data))
	    (tmp " *eplot convert*")
	    (executable (if width "rsvg-convert" "convert"))
	    sfile ofile)
	(unless svg
	  (error "Invalid image in the current buffer"))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (svg-print svg)
	  (if (string-match-p "\\.svg\\'" file)
	      (write-region (point-min) (point-max) file)
	    (if (and (string-match-p "\\.png\\'" file)
		     (executable-find "rsvg-convert"))
		(setq executable "rsvg-convert")
	      (unless (executable-find executable)
		(error "%s isn't installed; can only save svg files"
		       executable)))
	    (when (and (equal executable "rsvg-convert")
		       (not (string-match-p "\\.png\\'" file))
		       (not (executable-find "convert")))
	      (error "Can only write PNG files when scaling because \"convert\" isn't installed"))
	    (unwind-protect
		(progn
		  (setq sfile (make-temp-file "eplot" nil ".svg")
			ofile (make-temp-file "eplot" nil ".png"))
		  (write-region (point-min) (point-max) sfile nil 'silent)
		  ;; We don't use `call-process-region', because
		  ;; convert doesn't seem to like that?
		  (let ((code (if (equal executable "rsvg-convert")
				  (apply
				   #'call-process
				   executable nil (get-buffer-create tmp) nil
				   `(,(format "--output=%s"
					      (expand-file-name ofile))
				     ,@(and width
					    `(,(format "--width=%d" width)
					      "--keep-aspect-ratio"))
				     ,sfile))
				(call-process
				 executable nil (get-buffer-create tmp) nil
				 sfile file))))
		    (eplot--view-error code tmp)
		    (when (file-exists-p ofile)
		      (if (string-match-p "\\.png\\'" file)
			  (rename-file ofile file)
			(let ((code (call-process "convert" nil tmp nil
						  ofile file)))
			  (eplot--view-error code tmp))))
		    (message "Wrote %s" file)))
	      ;; Clean-up.
	      (when (get-buffer tmp)
		(kill-buffer tmp))
	      (when (file-exists-p sfile)
		(delete-file sfile))
	      (when (file-exists-p ofile)
		(delete-file sfile)))))))))

(defun eplot--view-error (code tmp)
  (unless (zerop code)
    (error "Error code %d: %s"
	   code
	   (with-current-buffer tmp
	     (while (search-forward "[ \t\n]+" nil t)
	       (replace-match " "))
	     (string-trim (buffer-string))))))

(defun eplot-view-write-scaled-file (width file)
  "Write the current chart to a rescaled to a file.
The rescaling is done by \"rsvg-convert\", which has to be
installed.  Rescaling is done when rendering, so this should give
you a clear, non-blurry version of the chart at any size."
  (interactive "nWidth: \nFWrite to file: ")
  (eplot-view-write-file file width))

(defvar eplot--data-buffer nil)

(defun eplot ()
  "Plot the data in the current buffer."
  (interactive)
  (eplot-update-view-buffer))

(defun eplot-with-headers (header-file)
  "Plot the data in the current buffer using headers from a file."
  (interactive "fHeader file: ")
  (eplot-update-view-buffer
   (with-temp-buffer
     (insert-file-contents header-file)
     (eplot--parse-headers))))

(defun eplot-update-view-buffer (&optional headers)
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
    (let* ((eplot--user-defaults (eplot--settings-table))
	   (data (eplot--parse-buffer))
	   (data-buffer (current-buffer)))
      (unless data
	(user-error "No data in the current buffer"))
      (setq data (eplot--inject-headers data headers))
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

(defun eplot--settings-table ()
  (cond
   ((not eplot--transient-settings)
    eplot--user-defaults)
   (t
    ;; We have to merge the tables.
    (let ((table (make-hash-table)))
      (maphash (lambda (k v) (setf (gethash k table) v))
	       eplot--user-defaults)
      (maphash (lambda (k v) (setf (gethash k table) v))
	       eplot--transient-settings)
      table))))

(defun eplot--inject-headers (data headers)
  ;; It's OK not to separate the plot headers from the chart
  ;; headers.  Collect them here, if any.
  (when-let ((plot-headers
	      (cl-loop for elem in (mapcar #'car eplot--plot-headers)
		       for value = (eplot--vs elem headers)
		       when value
		       collect (progn
				 ;; Remove these headers from the data
				 ;; headers so that we don't get errors
				 ;; on undefined headers.
				 (setq headers (delq (assq elem headers)
						     headers))
				 (cons elem value)))))
    (dolist (plot (cdr (assq :plots data)))
      (let ((headers (assq :headers plot)))
	(if headers
	    (nconc headers plot-headers)
	  (nconc plot (list (list :headers plot-headers)))))))
  (append data headers))

(defun eplot-eval-and-update ()
  "Helper command when developing."
  (interactive nil emacs-lisp-mode)
  (save-some-buffers t)
  (elisp-eval-buffer)
  (eval-defun nil)
  (eplot-update-view-buffer))

;;; Parsing buffers.

(defun eplot-update (&rest _ignore)
  "Update the plot in the current buffer."
  (interactive)
  (unless eplot--data-buffer
    (user-error "No data buffer associated with this eplot view buffer"))
  (let ((data (with-current-buffer eplot--data-buffer
		(eplot--parse-buffer)))
	(inhibit-read-only t))
    (erase-buffer)
    (eplot--render data)
    (insert "\n\n")))

(defun eplot--parse-buffer ()
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf)
      ;; Remove comments first.
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#" nil t)
	(delete-line))
      (goto-char (point-min))
      ;; First headers.
      (let* ((data (eplot--parse-headers))
	     (plot-headers
	      ;; It's OK not to separate the plot headers from the chart
	      ;; headers.  Collect them here, if any.
	      (cl-loop for elem in (mapcar #'car eplot--plot-headers)
		       for value = (eplot--vs elem data)
		       when value
		       collect (progn
				 ;; Remove these headers from the data
				 ;; headers so that we don't get errors
				 ;; on undefined headers.
				 (setq data (delq (assq elem data) data))
				 (cons elem value))))
	     plots)
	;; Then the values.
	(while-let ((plot (eplot--parse-values nil plot-headers)))
	  (setq plot-headers nil)
	  (push plot plots))
	(when plots
	  (push (cons :plots (nreverse plots)) data))
	data))))

(defun eplot--parse-headers ()
  (let ((data nil)
	type value)
    (while (looking-at "\\([^\n\t :]+\\):\\(.*\\)")
      (setq type (intern (downcase (match-string 1)))
	    value (substring-no-properties (string-trim (match-string 2))))
      (forward-line 1)
      ;; Get continuation lines.
      (while (looking-at "[ \t]+\\(.*\\)")
	(setq value (concat value " " (string-trim (match-string 1))))
	(forward-line 1))
      (if (eq type 'header-file)
	  (setq data (nconc data
			    (with-temp-buffer
			      (insert-file-contents value)
			      (eplot--parse-headers))))
	;; We don't use `push' here because we want to preserve order
	;; also when inserting headers from other files.
	(setq data (nconc data (list (cons type value))))))
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
    (if-let ((data-file (eplot--vs 'data-file headers)))
	(with-temp-buffer
	  (insert-file-contents data-file)
	  (setq values (cdr (assq :values (eplot--parse-values headers)))
		headers (delq (assq 'data headers) headers)))
      ;; Now we come to the data.  The data is typically either just a
      ;; number, or two numbers (in which case the first number is a
      ;; date or a time).  Labels ans settings can be introduced with
      ;; a # char.
      (while (looking-at "\\([-0-9. \t]+\\)\\([ \t]*#\\(.*\\)\\)?")
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
	  (when numbers
	    (setq this (nconc this (list :value (pop numbers)))))
	  (when two-values
	    (setq this (nconc this (list :extra-value (pop numbers)))))
	  (when settings
	    (setq this (nconc this (list :settings settings))))
	  (when (plist-get this :value)
	    (push this values)))
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

;;; Accessing data.

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

(defvar eplot--chart-headers nil)

(defmacro eplot-def (args doc-string)
  (declare (indent defun))
  `(eplot--def ',(nth 0 args) ',(nth 1 args) ',(nth 2 args) ',(nth 3 args)
	       ,doc-string))

(defun eplot--def (name type default valid doc)
  (setq eplot--chart-headers (delq (assq name eplot--chart-headers)
				   eplot--chart-headers))
  (push (list name
	      :type type
	      :default default
	      :doc doc
	      :valid valid)
	eplot--chart-headers))

(eplot-def (width number)
  "The width of the entire chart.")

(eplot-def (height number)
  "The height of the entire chart.")

(eplot-def (format symbol normal (normal bar-chart))
  "The overall format of the chart.
Possible values are `normal' and `bar-chart'.")

(eplot-def (layout symbol (normal compact))
  "The general layout of the chart.
Possible values are `normal' and `compact'.")

(eplot-def (mode symbol light (dark light))
  "Dark/light mode.
Possible values are `dark' and `light'.")

(eplot-def (margin-left number 70)
  "The left margin.")

(eplot-def (margin-right number 20)
  "The right margin.")

(eplot-def (margin-top number 40)
  "The top margin.")

(eplot-def (margin-bottom number 60)
  "The bottom margin.")

(eplot-def (x-axis-label-space number 5)
  "The space between the X axis and the label.")

(eplot-def (font string "sans-serif")
  "The font to use in titles, labels and legends.")

(eplot-def (font-size number 12)
  "The font size.")

(eplot-def (chart-color string "black")
  "The foreground color to use in plots, axes, legends, etc.
This is used as the default, but can be overridden per thing.")

(eplot-def (background-color string "white")
  "The background color.
If you want a chart with a transparent background, use the color
\"none\".")

(eplot-def (background-gradient string)
  "Use this to get a gradient color in the background.")

(eplot-def (axes-color string (spec chart-color))
  "The color of the axes.")

(eplot-def (grid-color string "#e0e0e0")
  "The color of the grid.")

(eplot-def (grid symbol xy (xy x y off))
  "What grid axes to do.
Possible values are `xy', `x', `y' and `off'.")

(eplot-def (grid-opacity number)
  "The opacity of the grid.
This should either be nil or a value between 0 and 1, where 0 is
fully transparent.")

(eplot-def (grid-position symbol bottom (bottom top))
  "Whether to put the grid on top or under the plot.
Possible values are `bottom' and `top''.")

(eplot-def (legend symbol nil (t nil))
  "Whether to do a legend.")

(eplot-def (legend-color string (spec chart-color))
  "The color of legends (if any).")

(eplot-def (legend-border-color string (spec chart-color))
  "The border color of legends (if any).")

(eplot-def (legend-background-color string (spec background-color))
  "The background color of legends (if any).")

(eplot-def (label-color string (spec axes-color))
  "The color of labels on the axes.")

(eplot-def (surround-color string)
  "The color between the plot area and the edges of the chart.")

(eplot-def (border-color string)
  "The color of the border of the chart, if any.")

(eplot-def (border-width number)
  "The width of the border of the chart, if any.")

(eplot-def (frame-color string)
  "The color of the frame of the plot, if any.")

(eplot-def (frame-width number)
  "The width of the frame of the plot, if any.")

(eplot-def (min number)
  "The minimum value in the chart.
This is normally computed automatically, but can be overridden
 with this spec.")

(eplot-def (max number)
  "The maximum value in the chart.
This is normally computed automatically, but can be overridden
 with this spec.")

(eplot-def (title string)
  "The title of the chart, if any.")

(eplot-def (title-color string (spec chart-color))
  "The color of the title.")

(eplot-def (x-label string)
  "The label of the X axis, if any.")

(eplot-def (y-label string)
  "The label of the X axis, if any.")

(eplot-def (background-image-file string)
  "Use an image as the background.")

(eplot-def (background-image-opacity number 1)
  "The opacity of the background image.")

(eplot-def (background-image-cover symbol all (all plot frame))
  "Position of the background image.
Valid values are `all' (the entire image), `plot' (the plot area)
and `frame' (the surrounding area).")

(eplot-def (header-file string)
  "File where the headers are.")

(defvar eplot-compact-defaults
  '((margin-left 30)
    (margin-right 10)
    (margin-top 20)
    (margin-bottom 21)
    (font-size 12)
    (x-axis-label-space 3)))

(defvar eplot-dark-defaults
  '((chart-color "#c0c0c0")
    (axes-color "#c0c0c0")
    (grid-color "#404040")
    (background-color "#101010")
    (label-color "#c0c0c0")
    (legend-color "#c0c0c0")))

(defvar eplot-bar-chart-defaults
  '((grid-position top)
    (grid y)
    (grid-opacity 0.2)
    (min 0)))

(defclass eplot-chart ()
  (
   (plots :initarg :plots)
   (xs)
   (ys)
   (x-values :initform nil)
   (x-type :initform nil)
   (x-min)
   (x-max)
   (x-ticks)
   (y-ticks)
   (stride)
   (print-format)
   (x-tick-step)
   (x-label-step)
   (x-step-map :initform nil)
   (y-tick-step)
   (y-label-step)
   (inhibit-compute-x-step :initform nil)
   (set-min :initform nil)
   (set-max :initform nil)
   ;; ---- CUT HERE ----
   (axes-color :initarg :axes-color :initform nil)
   (background-color :initarg :background-color :initform nil)
   (background-gradient :initarg :background-gradient :initform nil)
   (background-image-file :initarg :background-image-file :initform nil)
   (background-image-opacity :initarg :background-image-opacity :initform nil)
   (background-image-cover :initarg :background-image-cover :initform nil)
   (border-color :initarg :border-color :initform nil)
   (border-width :initarg :border-width :initform nil)
   (chart-color :initarg :chart-color :initform nil)
   (font :initarg :font :initform nil)
   (font-size :initarg :font-size :initform nil)
   (format :initarg :format :initform nil)
   (frame-color :initarg :frame-color :initform nil)
   (frame-width :initarg :frame-width :initform nil)
   (grid :initarg :grid :initform nil)
   (grid-color :initarg :grid-color :initform nil)
   (grid-opacity :initarg :grid-opacity :initform nil)
   (grid-position :initarg :grid-position :initform nil)
   (header-file :initarg :header-file :initform nil)
   (height :initarg :height :initform nil)
   (label-color :initarg :label-color :initform nil)
   (layout :initarg :layout :initform nil)
   (legend :initarg :legend :initform nil)
   (legend-color :initarg :legend-color :initform nil)
   (legend-background-color :initarg :legend-background-color :initform nil)
   (legend-border-color :initarg :legend-border-color :initform nil)
   (margin-bottom :initarg :margin-bottom :initform nil)
   (margin-left :initarg :margin-left :initform nil)
   (margin-right :initarg :margin-right :initform nil)
   (margin-top :initarg :margin-top :initform nil)
   (mode :initarg :mode :initform nil)
   (surround-color :initarg :surround-color :initform nil)
   (title :initarg :title :initform nil)
   (title-color :initarg :title-color :initform nil)
   (width :initarg :width :initform nil)
   (x-axis-label-space :initarg :x-axis-label-space :initform nil)
   (x-label :initarg :x-label :initform nil)
   (y-label :initarg :y-label :initform nil)
   (max :initarg :max :initform nil)
   (min :initarg :min :initform nil)
   ;; ---- CUT HERE ----
   ))

;;; Parameters that are plot specific.

(defvar eplot--plot-headers nil)

(defmacro eplot-pdef (args doc-string)
  (declare (indent defun))
  `(eplot--pdef ',(nth 0 args) ',(nth 1 args) ',(nth 2 args) ',(nth 3 args)
		,doc-string))

(defun eplot--pdef (name type default valid doc)
  (setq eplot--plot-headers (delq (assq name eplot--plot-headers)
				   eplot--plot-headers))
  (push (list name
	      :type type
	      :default default
	      :valid valid
	      :doc doc)
	eplot--plot-headers))

(eplot-pdef (smoothing symbol nil (moving-average nil))
  "Smoothing algorithm to apply to the data, if any.
Valid values are `moving-average' and, er, probably more to come.")

(eplot-pdef (gradient string)
  "Gradient to apply to the plot.
The syntax is:

  from-color to-color direction position

The last two parameters are optional.

direction is either `top-down' (the default), `bottom-up',
`left-right' or `right-left').

position is either `below' or `above'.

to-color can be either a color name, or a string that defines
stops and colors:

   Gradient: black 25-purple-50-white-75-purple-black

In that case, the second element specifies the percentage points
of where each color ends, so the above starts with black, then at
25% it's purple, then at 50% it's white, then it's back to purple
again at 75%, before ending up at black at a 100% (but you don't
have to include the 100% here -- it's understood).")

(eplot-pdef (style symbol line ( line impulse point square cricle cross
				 triangle rectangle))
  "Style the plot should be drawn in.
Valid values are listed below.  Some styles take additional
optional parameters.

line

impulse
  size: width of the impulse

point

square

circle
  size: diameter of the circle
  fill-color: color to fill the center

cross
  size: length of the lines in the cross

triangle
  size: length of the sides of the triangle
  fill-color: color to fill the center

rectangle
  size: length of the sides of the rectangle
  fill-color: color to fill the center")

(eplot-pdef (fill-color string)
  "Color to use to fill the plot styles that are closed shapes.
I.e., circle, triangle and rectangle.")

(eplot-pdef (color string (spec chart-color))
  "Color to draw the plot.")

(eplot-pdef (data-format symbol single (single date time xy))
  "Format of the data.
By default, eplot assumes that each line has a single data point.
This can also be `date', `time' and `xy'.

date: The first column is a date on ISO8601 format (i.e., YYYYMMDD).

time: The first column is a clock (i.e., HHMMSS).

xy: The first column is the X position.")

(eplot-pdef (data-column number 1)
  "Column where the data is.")

(eplot-pdef (fill-border-color string)
  "Border around the fill area when using a fill/gradient style.")

(eplot-pdef (size number)
  "Size of elements in styles that have meaningful sizes.")

(eplot-pdef (data-file string)
  "File where the data is.")

(eplot-pdef (data-format symbol-list nil (nil two-values date time))
  "List of symbols to describe the data format.
Elements allowed are `two-values', `date' and `time'.")

(eplot-pdef (name string)
  "Name of the plot, which will be displayed if legends are switched on.")

(eplot-pdef (legend-color string (spec chart-color))
  "Color for the name to be displayed in the legend.")

(defclass eplot-plot ()
  (
   (values :initarg :values)
   ;; ---- CUT HERE ----
   (color :initarg :color :initform nil)
   (data-column :initarg :data-column :initform nil)
   (data-file :initarg :data-file :initform nil)
   (data-format :initarg :data-format :initform nil)
   (fill-border-color :initarg :fill-border-color :initform nil)
   (fill-color :initarg :fill-color :initform nil)
   (gradient :initarg :gradient :initform nil)
   (legend-color :initarg :legend-color :initform nil)
   (name :initarg :name :initform nil)
   (size :initarg :size :initform nil)
   (smoothing :initarg :smoothing :initform nil)
   (style :initarg :style :initform nil)
   ;; ---- CUT HERE ----
   ))

(defun eplot--make-plot (data)
  "Make an `eplot-plot' object and initialize based on DATA."
  (let ((plot (make-instance 'eplot-plot
			     :values (cdr (assq :values data)))))
    ;; Get the program-defined defaults.
    (eplot--object-defaults plot eplot--plot-headers)
    (eplot--object-values plot (cdr (assq :headers data)) eplot--plot-headers)
    plot))

(defun eplot--make-chart (data)
  "Make an `eplot-chart' object and initialize based on DATA."
  (let ((chart (make-instance 'eplot-chart
			      :plots (mapcar #'eplot--make-plot
					     (eplot--vs :plots data)))))
    ;; First get the program-defined defaults.
    (eplot--object-defaults chart eplot--chart-headers)
    ;; Then do the "meta" variables.
    (when (eq (eplot--vy 'mode data) 'dark)
      (eplot--set-theme chart eplot-dark-defaults))
    (when (eq (eplot--vy 'layout data) 'compact)
      (eplot--set-theme chart eplot-compact-defaults))
    (when (eq (eplot--vy 'format data) 'bar-chart)
      (eplot--set-theme chart eplot-bar-chart-defaults))
    ;; Set defaults from user settings/transients.
    (maphash (lambda (name value)
	       (setf (slot-value chart name) value))
	     eplot--user-defaults)
    ;; Finally, use the data from the chart.
    (eplot--object-values chart data eplot--chart-headers)
    ;; Note when min/max are explicitly set.
    (with-slots (min max set-min set-max) chart
      (setq set-min min
	    set-max max))
    chart))

(defun eplot--object-defaults (object headers)
  (dolist (header headers)
    (when-let ((default (plist-get (cdr header) :default)))
      (setf (slot-value object (car header))
	    ;; Allow overrides via `eplot-set'.
	    (or (gethash (car header) eplot--user-defaults)
		(if (and (consp default)
			 (eq (car default) 'spec))
		    ;; Chase dependencies.
		    (eplot--default (cadr default))
		  default))))))

(defun eplot--object-values (object data headers)
  (cl-loop for (name . value) in data
	   do (unless (eq name :plots)
		(let ((spec (cdr (assq name headers))))
		  (if (not spec)
		      (error "%s is not a valid spec" name)
		    (let ((value 
			   (cl-case (plist-get spec :type)
			     (number
			      (string-to-number value))
			     (symbol
			      (intern (downcase value)))
			     (symbol-list
			      (mapcar #'intern (split-string (downcase value))))
			     (t
			      value))))
		      (setf (slot-value object name) value)
		      (eplot--set-dependent-values object name value)))))))

(defun eplot--set-dependent-values (object name value)
  (dolist (slot (gethash name (eplot--dependecy-graph)))
    (setf (slot-value object slot) value)
    (eplot--set-dependent-values object slot value)))

(defun eplot--set-theme (chart map)
  (cl-loop for (slot value) in map
	   do (setf (slot-value chart slot) value)))

(defun eplot--default (slot)
  "Find the default value for SLOT, chasing dependencies."
  (let ((spec (cdr (assq slot eplot--chart-headers))))
    (unless spec
      (error "Invalid slot %s" slot))
    (let ((default (plist-get spec :default)))
      (if (and (consp default)
	       (eq (car default) 'spec))
	  (eplot--default (cadr default))
	default))))

(defun eplot--dependecy-graph ()
  (let ((table (make-hash-table)))
    (dolist (elem eplot--chart-headers)
      (let ((default (plist-get (cdr elem) :default)))
	(when (and (consp default)
		   (eq (car default) 'spec))
	  (push (car elem) (gethash (cadr default) table)))))
    table))

(defun eplot--render (data &optional return-image)
  "Create the chart and display it.
If RETURN-IMAGE is non-nil, return it instead of displaying it."
  (let* ((chart (eplot--make-chart data))
	 svg)
    (with-slots ( width height xs ys
		  margin-left margin-right margin-top margin-bottom
		  grid-position plots)
	chart
      ;; Set the size of the chart based on the window it's going to
      ;; be displayed in.  It uses the *eplot* window by default, or
      ;; the current one if that isn't displayed.
      (let ((factor (image-compute-scaling-factor)))
	(unless width
	  (setq width (/ (* (window-pixel-width
 			     (get-buffer-window "*eplot*" t))
			    0.9)
			 factor)))
	(unless height
	  (setq height (/ (* (window-pixel-height
			      (get-buffer-window "*eplot*" t))
			     0.9)
			  factor))))
      (setq svg (svg-create width height)
	    xs (- width margin-left margin-right)
	    ys (- height margin-top margin-bottom))
      ;; Draw background/borders/titles/etc.
      (eplot--draw-basics svg chart)

      ;; Protect against being called in an empty buffer.
      (when plots
	;; Compute min/max based on all plots, and also compute x-ticks
	;; etc.
	(eplot--compute-chart-dimensions chart)
	;; Analyze values and adjust values accordingly.
	(eplot--adjust-chart chart)
    
	(when (eq grid-position 'top)
	  (eplot--draw-plots svg chart))

	(eplot--draw-x-ticks svg chart)
	(eplot--draw-y-ticks svg chart)
      
	;; Draw axes.
	(with-slots ( margin-left margin-right margin-margin-top
		      margin-bottom axes-color)
	    chart
	  (svg-line svg margin-left margin-top margin-left
		    (+ (- height margin-bottom) 5)
		    :stroke axes-color)
	  (svg-line svg (- margin-left 5) (- height margin-bottom)
		    (- width margin-right) (- height margin-bottom)
		    :stroke axes-color))
    
	(when (eq grid-position 'bottom)
	  (eplot--draw-plots svg chart)))

      (with-slots (frame-color frame-width) chart
	(when (or frame-color frame-width)
	  (svg-rectangle svg margin-left margin-top xs ys
			 :stroke-width frame-width
			 :fill "none"
			 :stroke-color frame-color)))
      (eplot--draw-legend svg chart))

    (if return-image
	svg
      (svg-insert-image svg))))

(defun eplot--draw-basics (svg chart)
  (with-slots ( width height 
		chart-color font font-size
		margin-left margin-right margin-top margin-bottom
		background-color label-color
		xs ys)
      chart
    ;; Add background.
    (eplot--draw-background chart svg 0 0 width height)
    (with-slots ( background-image-file background-image-opacity
		  background-image-cover)
	chart
      (when background-image-file
	(apply #'svg-embed svg background-image-file "image/jpeg" nil
	       :opacity background-image-opacity
	       :preserveAspectRatio "xMidYMid slice"
	       (if (memq background-image-cover '(all frame))
		   `(:x 0 :y 0 :width ,width :height ,height)
		 `(:x ,margin-left :y ,margin-top :width ,xs :height ,ys)))
	(when (eq background-image-cover 'frame)
	  (eplot--draw-background chart svg margin-left margin-right xs ys))))
    ;; Area between plot and edges.
    (with-slots (surround-color) chart
      (when surround-color
	(svg-rectangle svg 0 0 width height
		       :fill surround-color)
	(svg-rectangle svg margin-left margin-top
		       xs ys
		       :fill background-color)))
    ;; Border around the entire chart.
    (with-slots (border-width border-color) chart
      (when (or border-width border-color)
	(svg-rectangle svg 0 0 width height
		       :stroke-width (or border-width 1)
		       :fill "none"
		       :stroke-color (or border-color chart-color))))
    ;; Frame around the plot.
    (with-slots (frame-width frame-color) chart
      (when (or frame-width frame-color)
	(svg-rectangle svg margin-left margin-top xs ys
		       :stroke-width (or frame-width 1)
		       :fill "none"
		       :stroke-color (or frame-color chart-color))))
    ;; Title and legends.
    (with-slots (title title-color) chart
      (when title
	(svg-text svg title
		  :font-family font
		  :text-anchor "middle"
		  :font-weight "bold"
		  :font-size font-size
		  :fill title-color
		  :x (+ margin-left (/ (- width margin-left margin-right) 2))
		  :y (+ 3 (/ margin-top 2)))))
    (with-slots (x-label) chart
      (when x-label
	(svg-text svg x-label
		  :font-family font
		  :text-anchor "middle"
		  :font-weight "bold"
		  :font-size font-size
		  :fill label-color
		  :x (+ margin-left (/ (- width margin-left margin-right) 2))
		  :y (- height (/ margin-bottom 4)))))
    (with-slots (y-label) chart
      (when y-label
	(svg-text svg y-label
		  :font-family font
		  :text-anchor "middle"
		  :font-weight "bold"
		  :font-size font-size
		  :fill label-color
		  :transform
		  (format "translate(%s,%s) rotate(-90)"
			  (- (/ margin-left 2) (/ font-size 2))
			  (+ margin-top
			     (/ (- height margin-bottom margin-top) 2))))))))

(defun eplot--draw-background (chart svg left top width height)
  (with-slots (background-gradient background-color) chart
    (let ((gradient (eplot--parse-gradient background-gradient))
	  id)
      (when gradient
	(setq id (format "gradient-%s" (make-temp-name "grad")))
	(eplot--gradient svg id 'linear
			 (eplot--stops (eplot--vs 'from gradient)
				       (eplot--vs 'to gradient))
			 (eplot--vs 'direction gradient)))
      (apply #'svg-rectangle svg left top width height
	     (if gradient
		 `(:gradient ,id)
	       `(:fill ,background-color))))))

(defun eplot--compute-chart-dimensions (chart)
  (with-slots ( min max plots x-values x-min x-max x-ticks stride
		print-format font-size
		xs
		inhibit-compute-x-step x-type x-step-map format
		x-tick-step x-label-step)
      chart
    (let ((set-min min)
	  (set-max max))
      (dolist (plot plots)
	(with-slots (values data-format) plot
	  (let* ((vals (nconc (seq-map (lambda (v) (plist-get v :value)) values)
			      (and (memq 'two-values data-format)
				   (seq-map
				    (lambda (v) (plist-get v :extra-value))
				    values)))))
	    ;; Set the x-values based on the first plot.
	    (unless x-values
	      (setq print-format (cond
				  ((memq 'date data-format) 'date)
				  ((memq 'time data-format) 'time)
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
		      stride (e/ xs (- x-max x-min))
		      inhibit-compute-x-step t)
		(let ((xs (eplot--get-date-ticks x-min x-max xs font-size)))
		  (setq x-ticks (car xs)
			print-format (cadr xs)
			x-tick-step 1
			x-label-step 1
			x-step-map (nth 2 xs))))
	       ((memq 'time data-format)
		(setq x-values
		      (cl-loop for val in values
			       collect
			       (time-convert
				(encode-time
				 (decoded-time-set-defaults
				  (iso8601-parse-time
				   (format "%06d" (plist-get val :x)))))
				'integer))
		      x-min (car x-values)
		      x-max (car (last x-values))
		      stride (e/ xs (- x-max x-min))
		      inhibit-compute-x-step t)
		(let ((xs (eplot--get-time-ticks x-min x-max xs font-size)))
		  (setq x-ticks (car xs)
			print-format (cadr xs)
			x-tick-step 1
			x-label-step 1
			x-step-map (nth 2 xs))))
	       (t
		;; This is a one-dimensional plot -- we don't have X
		;; values, really, so we just do zero to (1- (length
		;; values)).
		(setq x-type 'one-dimensional
		      stride (e/ xs
				 ;; Fenceposting bar-chart vs everything else.
				 (if (eq format 'bar-chart)
				     (length values)
				   (1- (length values))))
		      x-values (cl-loop for i from 0
					repeat (length values)
					collect i)
		      x-min (car x-values)
		      x-max (car (last x-values))
		      x-ticks x-values))))
	    (unless set-min
	      (setq min (min (or min 1.0e+INF) (seq-min vals))))
	    (unless set-max
	      (setq max (max (or max -1.0e+INF) (seq-max vals))))))))))

(defun eplot--adjust-chart (chart)
  (with-slots ( x-tick-step x-label-step y-tick-step y-label-step
		min max ys format inhibit-compute-x-step
		y-ticks xs x-values font-size print-format
		set-min set-max)
      chart
    (setq y-ticks (and max
		       (eplot--get-ticks
			min
			;; We get 2% more ticks to check whether we
			;; should extend max.
			(if set-max max (* max 1.02))
			ys)))
    (if (eq format 'bar-chart)
	(setq x-tick-step 1
	      x-label-step 1)
      (unless inhibit-compute-x-step
	(let ((xt (eplot--compute-x-ticks
		   xs x-values font-size print-format)))
	  (setq x-tick-step (car xt)
		x-label-step (cadr xt)))))
    (when max
      (let ((yt (eplot--compute-y-ticks ys y-ticks font-size)))
	(setq y-tick-step (car yt)
	      y-label-step (cadr yt))))
    ;; If max is less than 2% off from a pleasant number, then
    ;; increase max.
    (unless set-max
      (cl-loop for tick in (reverse y-ticks)
	       when (and (< max tick)
			 (< (e/ (- tick max) (- max min)) 0.02))
	       return (progn
			(setq max tick)
			;; Chop off any further ticks.
			(setcdr (member tick y-ticks) nil))))

    (when y-ticks
      (if (and (not set-min)
	       (< (car y-ticks) min))
	  (setq min (car y-ticks))
	;; We may be extending the bottom of the chart to get pleasing
	;; numbers.  We don't want to be drawing the chart on top of the
	;; X axis, because the chart won't be visible there.
	(when (and nil
		   (<= min (car y-ticks))
		   ;; But not if we start at origo, because that just
		   ;; looks confusing.
		   (not (zerop min)))
	  (setq min (- (car y-ticks)
		       ;; 2% of the value range.
		       (* 0.02 (- (car (last y-ticks)) (car y-ticks))))))))))

(defun eplot--draw-x-ticks (svg chart)
  (with-slots ( x-step-map x-ticks format layout print-format
		margin-left margin-right margin-top margin-bottom
		x-min x-max xs stride
		width height
		axes-color label-color
		grid grid-opacity grid-color
		font font-size x-tick-step x-label-step
		plots)
      chart
    ;; Make X ticks.
    (cl-loop for xv in (or x-step-map x-ticks)
	     for x = (if (consp xv) (car xv) xv)
	     for do-tick = (if (consp xv)
			       (nth 1 xv)
			     (zerop (e% x x-tick-step)))
	     for do-label = (if (consp xv)
				(nth 2 xv)
			      (zerop (e% x x-label-step)))
	     for i from 0
	     for value = (and (equal format 'bar-chart)
			      (elt (slot-value (car plots) 'values) i))
	     for label = (if (equal format 'bar-chart)
			     (eplot--vs 'label
					(plist-get value :settings)
					;; When we're doing bar charts, we
					;; want default labeling to start with
					;; 1 and not zero.
					(format "%s" (1+ x)))
			   (eplot--format-value x print-format))
	     for px = (if (equal format 'bar-chart)
			  (+ margin-left (* x stride) (/ stride 2)
			     (/ (* stride 0.1) 2))
			(+ margin-left
			   (* (/ (- (* 1.0 x) x-min) (- x-max x-min))
			      xs)))
	     ;; We might have one extra stride outside the area -- don't
	     ;; draw it.
	     when (<= px (- width margin-right))
	     do
	     (when do-tick
	       ;; Draw little tick.
	       (unless (equal format 'bar-chart)
		 (svg-line svg
			   px (- height margin-bottom)
			   px (+ (- height margin-bottom)
				 (if do-label
				     4
				   2))
			   :stroke axes-color))
	       (when (or (eq grid 'xy) (eq grid 'x))
		 (svg-line svg px margin-top
			   px (- height margin-bottom)
			   :opacity grid-opacity
			   :stroke grid-color)))
	     (when (and do-label
			;; We want to skip marking the first X value
			;; unless we're a bar chart or we're a one
			;; dimensional chart.
			(or (equal format 'bar-chart)
			    t
			    (not (= x-min (car x-values)))
			    (eq x-type 'one-dimensional)
			    (and (not (zerop x)) (not (zerop i)))))
	       (svg-text svg label
			 :font-family font
			 :text-anchor "middle"
			 :font-size font-size
			 :fill label-color
			 :x px
			 :y (+ (- height margin-bottom)
			       font-size
			       (if (equal format 'bar-chart)
				   (if (equal layout 'compact) 3 5)
				 2)))))))

(defun eplot--draw-y-ticks (svg chart)
  (with-slots ( y-ticks height width
		margin-left margin-right margin-top margin-bottom
		min max ys
		y-tick-step y-label-step
		grid grid-opacity grid-color
		font font-size axes-color label-color)
      chart
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
	       (when (or (eq grid 'xy) (eq grid 'y))
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
			   :fill label-color
			   :x (- margin-left 6)
			   :y (+ py (/ font-size 2) -2)))))))

(defun eplot--draw-legend (svg chart)
  (with-slots ( legend plots
		margin-left margin-top
		font font-size
		background-color axes-color
		legend-color legend-background-color legend-border-color)
      chart
    (when (eq legend 'true)
      (when-let ((names
		  (cl-loop for plot in plots
			   for name = (slot-value plot 'name)
			   when name
			   collect
			   (cons name (slot-value plot 'legend-color)))))
	(svg-rectangle svg (+ margin-left 20) (+ margin-top 20)
		       (format "%dex"
			       (+ 2
				  (seq-max (mapcar (lambda (name)
						     (length (car name)))
						   names))))
		       (* font-size (+ (length names) 2))
		       :font-size font-size
		       :fill-color legend-background-color
		       :stroke-color legend-border-color)
	(cl-loop for name in names
		 for i from 0
		 do (svg-text svg (car name)
			      :font-family font
			      :text-anchor "front"
			      :font-size font-size
			      :fill (or (cdr name) legend-color)
			      :x (+ margin-left 25)
			      :y (+ margin-top 40 (* i font-size))))))))

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
   ((eq print-format 'minute)
    (format-time-string "%H:%M" value))
   ((eq print-format 'hour)
    (format-time-string "%H" value))
   (t
    (format "%s" value))))

(defun eplot--compute-x-ticks (xs x-values font-size print-format
				  &optional use-value)
  (let* ((min (seq-min x-values))
	 (max (seq-max x-values))
	 (count (length x-values))
	 (max-print (eplot--format-value (or use-value max) print-format))
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
  (let* ((min (car y-values))
	 (max (car (last y-values)))
	 (count (length y-values))
	 ;; We want each label to be spaced at least as long apart as
	 ;; the length of the longest label, with room for two blanks
	 ;; in between.
	 (min-spacing (* font-size 1.1))
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
	      (while (> (/ (- max min) label-step) (/ ys min-spacing))
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

(defvar eplot--pleasing-numbers '(1 2 5 10))

(defun eplot--next-weed (weed)
  (let (digits series)
    (if (>= weed 1)
	(setq digits (truncate (log weed 10))
	      series (/ weed (expt 10 digits)))
      (setq digits (eplot--decimal-digits weed)
	    series (truncate (* weed (expt 10 digits)))))
    (let ((next (cadr (memq series eplot--pleasing-numbers))))
      (unless next
	(error "Invalid weed: %s" weed))
      (if (>= weed 1)
	  (* next (expt 10 digits))
	(e/ next (expt 10 digits))))))

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

(defun eplot--draw-plots (svg chart)
  (with-slots ( plots chart-color height format
		margin-bottom margin-left
		min max xs ys
		stride margin-top
		x-values x-min x-max)
      chart
    ;; Draw all the plots.
    (cl-loop for plot in (reverse plots)
	     for plot-number from 0
	     for values = (slot-value plot 'values)
	     for vals = (eplot--smooth
			 (seq-map (lambda (v) (plist-get v :value)) values)
			 (slot-value plot 'smoothing)
			 xs)
	     for polygon = nil
	     for gradient = (eplot--parse-gradient (slot-value plot 'gradient))
	     for lpy = nil
	     for lpx = nil
	     for style = (if (eq format 'bar-chart)
			     'bar
			   (slot-value plot 'style))
	     for bar-gap = (* stride 0.1)
	     for clip-id = (format "url(#clip-%d)" plot-number)
	     do
	     (svg--append
	      svg
	      (dom-node 'clipPath
			`((id . ,(format "clip-%d" plot-number)))
			(dom-node 'rect
				  `((x . ,margin-left)
				    (y . , margin-top)
				    (width . ,xs)
				    (height . ,ys)))))
	     (unless gradient
	       (when-let ((fill (slot-value plot 'fill-color)))
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
			   (eplot--vs 'color settings (slot-value plot 'color))
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
		 (if (not gradient)
		     (svg-rectangle
		      svg (+ px bar-gap) py
		      (- stride bar-gap) (- height margin-bottom py)
		      :clip-path clip-id
		      :fill color)
		   (let ((id (format "gradient-%s" (make-temp-name "grad"))))
		     (eplot--gradient svg id 'linear
				      (eplot--stops (eplot--vs 'from gradient)
						    (eplot--vs 'to gradient))
				      (eplot--vs 'direction gradient))
		     (svg-rectangle
		      svg (+ px bar-gap) py
		      (- stride bar-gap) (- height margin-bottom py)
		      :clip-path clip-id
		      :gradient id))))
		(impulse
		 (let ((width (eplot--vn 'size settings
					 (or (slot-value plot 'size) 1))))
		   (if (= width 1)
		       (svg-line svg
				 px py
				 px (- height margin-bottom)
				 :clip-path clip-id
				 :stroke color)
		     (svg-rectangle svg
				    (- px (e/ width 2)) py
				    width (- height py margin-bottom)
				    :clip-path clip-id
				    :fill color))))
		(point
		 (svg-line svg px py (1+ px) (1+ py)
			   :clip-path clip-id
			   :stroke color))
		(line
		 ;; If we're doing a gradient, we're just collecting
		 ;; points and will draw the polygon later.
		 (if gradient
		     (push (cons px py) polygon)
		   (when lpx
		     (svg-line svg lpx lpy px py
			       :clip-path clip-id
			       :stroke color))))
		(square
		 (if gradient
		     (progn
		       (when lpx
			 (push (cons lpx py) polygon))
		       (push (cons px py) polygon))
		   (when lpx
		     (svg-line svg lpx lpy px lpy
			       :clip-path clip-id
			       :stroke color)
		     (svg-line svg px lpy px py
			       :clip-path clip-id
			       :stroke color))))
		(circle
		 (svg-circle svg px py
			     (eplot--vn 'size settings
					(or (slot-value plot 'size) 3))
			     :clip-path clip-id
			     :stroke color
			     :fill (eplot--vary-color
				    (eplot--vs
				     'fill-color settings
				     (or (slot-value plot 'fill-color) "none"))
				    i)))
		(cross
		 (let ((s (or (slot-value plot 'size) 3)))
		   (svg-line svg (- px s) (- py s)
			     (+ px s) (+ py s)
			     :clip-path clip-id
			     :stroke color)
		   (svg-line svg (+ px s) (- py s)
			     (- px s) (+ py s)
			     :clip-path clip-id
			     :stroke color)))
		(triangle
		 (let ((s (or (slot-value plot 'size) 5)))
		   (svg-polygon svg
				(list
				 (cons (- px (e/ s 2)) (+ py (e/ s 2)))
				 (cons px (- py (e/ s 2)))
				 (cons (+ px (e/ s 2)) (+ py (e/ s 2))))
				:clip-path clip-id
				:stroke color
				:fill-color
				(or (slot-value plot 'fill-color) "none"))))
		(rectangle
		 (let ((s (or (slot-value plot 'size) 3)))
		   (svg-rectangle svg (- px (e/ s 2)) (- py (e/ s 2))
				  s s
				  :clip-path clip-id
				  :stroke color
				  :fill-color
				  (or (slot-value plot 'fill-color) "none")))))
	      (setq lpy py
		    lpx px))

	     ;; We're doing a gradient of some kind, so draw it now when
	     ;; we've collected the polygon.
	     (when polygon
	       ;; We have a "between" chart, so collect the data points
	       ;; from the "extra" values, too.
	       (when (memq 'two-values (slot-value plot 'data-format))
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
			      :clip-path clip-id
			      :gradient id
			      :stroke (slot-value plot 'fill-border-color))
		 (setq polygon nil))))))

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

(defun eplot--get-date-ticks (start end xs font-size &optional skip-until)
  (let* ((secs (* 60 60 24))
	 (sday (/ start secs))
	 (eday (/ end secs))
	 (duration (- eday sday))
	 (limits
	  (list
	   (list (/ 368 16) 'date
		 (lambda (_d) t))
	   (list (/ 368 4) 'date
		 ;; Collect Mondays.
		 (lambda (decoded)
		   (= (decoded-time-weekday decoded) 1)))
	   (list (/ 368 2) 'date
		 ;; Collect 1st and 15th.
		 (lambda (decoded)
		   (or (= (decoded-time-day decoded) 1)
		       (= (decoded-time-day decoded) 15))))
	   (list (* 368 2) 'date
		 ;; Collect 1st of every month.
		 (lambda (decoded)
		   (= (decoded-time-day decoded) 1)))
	   (list (* 368 4) 'date
		 ;; Collect every quarter.
		 (lambda (decoded)
		   (and (= (decoded-time-day decoded) 1)
			(memq (decoded-time-month decoded) '(1 4 7 10)))))
	   (list (* 368 8) 'date
		 ;; Collect every half year.
		 (lambda (decoded)
		   (and (= (decoded-time-day decoded) 1)
			(memq (decoded-time-month decoded) '(1 7)))))
	   (list 1.0e+INF 'year
		 ;; Collect every Jan 1st.
		 (lambda (decoded)
		   (and (= (decoded-time-day decoded) 1)
			(= (decoded-time-month decoded) 1)))))))
    ;; First we collect the potential ticks.
    (while (or (>= duration (caar limits))
	       (and skip-until (>= skip-until (caar limits))))
      (pop limits))
    (let* ((x-ticks (cl-loop for date from sday upto eday
			     for time = (* date secs)
			     for decoded = (decode-time time)
			     when (funcall (nth 2 (car limits)) decoded)
			     collect time))
	   (count (length x-ticks))
	   (print-format (nth 1 (car limits)))
	   (max-print (eplot--format-value (car x-ticks) print-format))
	   (min-spacing (* (+ (length max-print) 2) (e/ font-size 2))))
      (cond
       ;; We have room for every X value.
       ((< (* count min-spacing) xs)
	(list x-ticks print-format))
       ;; We have to prune X labels, but not grid lines.  (We shouldn't
       ;; have a grid line more than every 10 pixels.)
       ((< (* count 10) xs)
	(cond
	 ((not (cdr limits))
	  (eplot--year-ticks x-ticks xs font-size))
	 ;; The Mondays grid is special, because it doesn't resolve
	 ;; into any of the bigger limits evenly.
	 ((= (caar limits) (/ 368 4))
	  (list x-ticks 'date
		(cl-loop for val in x-ticks
			 for i from 0
			 ;; This logic is not correct -- it should compute
			 ;; the factor.
			 collect (list val t (zerop (% i 2))))))
	 (t
	  (pop limits)
	  (catch 'found
	    (while limits
	      (let ((candidate
		     (cl-loop for val in x-ticks
			      for decoded = (decode-time val)
			      collect (list val t
					    (not (not
						  (funcall (nth 2 (car limits))
							   decoded)))))))
		(setq print-format (nth 1 (car limits)))
		(let ((min-spacing (* (+ (length max-print) 2)
				      (e/ font-size 2))))
		  (when (< (* (seq-count (lambda (v) (nth 2 v)) candidate)
			      min-spacing)
			   xs)
		    (throw 'found (list x-ticks print-format candidate)))))
	      (pop limits))
	    (eplot--year-ticks x-ticks xs font-size)))))
       ;; We have to reduce both grid lines and labels.
       (t
	(eplot--get-date-ticks start end xs font-size (caar limits)))))))

(defun eplot--year-ticks (x-ticks xs font-size)
  (let* ((year-ticks (mapcar (lambda (time)
			       (decoded-time-year (decode-time time)))
			     x-ticks))
	 (xv (eplot--compute-x-ticks
	      xs year-ticks font-size 'year)))
    (let ((tick-step (car xv))
	  (label-step (cadr xv)))
      (list x-ticks 'year
	    (cl-loop for year in year-ticks
		     for val in x-ticks
		     collect (list val
				   (zerop (% year tick-step))
				   (zerop (% year label-step))))))))

(defun eplot--get-time-ticks (start end xs font-size &optional skip-until)
  (let* ((duration (- end start))
	 (limits
	  (list
	   (list (* 2 60) 'time
		 (lambda (_d) t))
	   (list (* 2 60 60) 'time
		 ;; Collect whole minutes.
		 (lambda (decoded)
		   (zerop (decoded-time-second decoded))))
	   (list (* 3 60 60) 'minute
		 ;; Collect five minutes.
		 (lambda (decoded)
		   (zerop (% (decoded-time-minute decoded) 5))))
	   (list (* 4 60 60) 'minute
		 ;; Collect fifteen minutes.
		 (lambda (decoded)
		   (and (zerop (decoded-time-second decoded))
			(memq (decoded-time-minute decoded) '(0 15 30 45)))))
	   (list (* 8 60 60) 'minute
		 ;; Collect half hours.
		 (lambda (decoded)
		   (and (zerop (decoded-time-second decoded))
			(memq (decoded-time-minute decoded) '(0 30)))))
	   (list 1.0e+INF 'hour
		 ;; Collect whole hours.
		 (lambda (decoded)
		   (and (zerop (decoded-time-second decoded))
			(zerop (decoded-time-minute decoded))))))))
    ;; First we collect the potential ticks.
    (while (or (>= duration (caar limits))
	       (and skip-until (>= skip-until (caar limits))))
      (pop limits))
    (let* ((x-ticks (cl-loop for time from start upto end
			     for decoded = (decode-time time)
			     when (funcall (nth 2 (car limits)) decoded)
			     collect time))
	   (count (length x-ticks))
	   (print-format (nth 1 (car limits)))
	   (max-print (eplot--format-value (car x-ticks) print-format))
	   (min-spacing (* (+ (length max-print) 2) (e/ font-size 2))))
      (cond
       ;; We have room for every X value.
       ((< (* count min-spacing) xs)
	(list x-ticks print-format))
       ;; We have to prune X labels, but not grid lines.  (We shouldn't
       ;; have a grid line more than every 10 pixels.)
       ;; If we're plotting just seconds, then just weed out some seconds.
       ((and (< (* count 10) xs)
	     (= (caar limits) (* 2 60)))
	(let ((xv (eplot--compute-x-ticks xs x-ticks font-size 'time)))
	  (let ((tick-step (car xv))
		(label-step (cadr xv)))
	    (list x-ticks 'time
		  (cl-loop for val in x-ticks
			   collect (list val
					 (zerop (% val tick-step))
					 (zerop (% val label-step))))))))
       ;; Normal case for pruning labels, but not grid lines.
       ((< (* count 10) xs)
	(if (not (cdr limits))
	    (eplot--hour-ticks x-ticks xs font-size)
	  (pop limits)
	  (catch 'found
	    (while limits
	      (let ((candidate
		     (cl-loop for val in x-ticks
			      for decoded = (decode-time val)
			      collect (list val t
					    (not (not
						  (funcall (nth 2 (car limits))
							   decoded)))))))
		(setq print-format (nth 1 (car limits)))
		(let ((min-spacing (* (+ (length max-print) 2)
				      (e/ font-size 2))))
		  (when (< (* (seq-count (lambda (v) (nth 2 v)) candidate)
			      min-spacing)
			   xs)
		    (throw 'found (list x-ticks print-format candidate)))))
	      (pop limits))
	    (eplot--hour-ticks x-ticks xs font-size))))
       ;; We have to reduce both grid lines and labels.
       (t
	(eplot--get-time-ticks start end xs font-size (caar limits)))))))

(defun eplot--hour-ticks (x-ticks xs font-size)
  (let* ((eplot--pleasing-numbers '(1 3 6 12))
	 (hour-ticks (mapcar (lambda (time)
			       (decoded-time-hour (decode-time time)))
			     x-ticks))
	 (xv (eplot--compute-x-ticks xs hour-ticks font-size 'year)))
    (let ((tick-step (car xv))
	  (label-step (cadr xv)))
      (list x-ticks 'hour
	    (cl-loop for hour in hour-ticks
		     for val in x-ticks
		     collect (list val
				   (zerop (% hour tick-step))
				   (zerop (% hour label-step))))))))

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

(defun eplot-parse-and-insert (file)
  "Parse and insert a file in the current buffer."
  (interactive "fEplot file: ")
  (let ((default-directory (file-name-directory file)))
    (eplot--render (with-temp-buffer
		     (insert-file-contents file)
		     (eplot--parse-buffer)))))

(defun eplot-list-chart-headers ()
  "Pop to a buffer showing all chart parameters."
  (interactive)
  (pop-to-buffer "*eplot help*")
  (let ((inhibit-read-only t))
    (special-mode)
    (erase-buffer)
    (insert "The following headers influence the overall\nlook of the chart:\n\n")
    (eplot--list-headers eplot--chart-headers)
    (ensure-empty-lines 2)
    (insert "The following headers are per plot:\n\n")
    (eplot--list-headers eplot--plot-headers)
    (goto-char (point-min))))

(defun eplot--list-headers (headers)
  (dolist (header (sort (copy-sequence headers) (lambda (e1 e2)
						  (string< (car e1) (car e2)))))
    (insert (propertize (capitalize (symbol-name (car header))) 'face 'bold)
	    "\n")
    (let ((start (point)))
      (insert (plist-get (cdr header) :doc) "\n")
      (indent-rigidly start (point) 2))
    (ensure-empty-lines 1)))

(defvar eplot--transients
  '((("Size"
      ("sw" "Width")
      ("sh" "Height")
      ("sml" "Margin-Left")
      ("smt" "Margin-Top")
      ("smr" "Margin-Right")
      ("smb" "Margin-Bottom"))
     ("Colors"
      ("ca" "Axes-Color")
      ("cb" "Border-Color")
      ("cc" "Chart-Color")
      ("cf" "Frame-Color")
      ("cs" "Surround-Color")
      ("ct" "Title-Color"))
     ("Background"
      ("bc" "Background-Color")
      ("bg" "Background-Gradient")
      ("bif" "Background-Image-File")
      ("bic" "Background-Image-Cover")
      ("bio" "Background-Image-Opacity")))
    (("General"
      ("bt" "Title")
      ("bf" "Font-Size")
      ("bf" "Font")
      ("bf" "Format")
      ("bf" "Frame-Width")
      ("bh" "Header-File")
      ("bd" "Data-File")
      ("bi" "Min")
      ("ba" "Max")
      ("bm" "Mode"))
     ("Legend, Axes & Grid"
      ("ll" "Legend")
      ("lb" "Legend-Background-Color")
      ("lo" "Legend-Borrder-Color")
      ("lc" "Legend-Color")
      ("xs" "X-Axis-Label-Space")
      ("xx" "X-Label")
      ("xy" "Y-label")
      ("gf" "Grid-Color")
      ("go" "Grid-Opacity")
      ("gp" "Grid-Position"))
     ("Plot"
      ("pc" "Color")
      ("pd" "Data-Column")
      ("pf" "Data-File")
      ("pf" "Data-format")
      ("pb" "Fill-Border-Color")
      ("pf" "Fill-Color")
      ("pg" "Gradient")
      ("pn" "Name")
      ("ps" "Size")
      ("po" "Smoothing")
      ("ps" "Style")))))

(defun eplot--define-transients ()
  (cl-loop for row in eplot--transients
	   collect (cl-coerce
		    (cl-loop for column in row
			     collect
			     (cl-coerce
			      (cons (pop column)
				    (mapcar #'eplot--define-transient column))
			      'vector))
		    'vector)))

(defun eplot--define-transient (action)
  (list (nth 0 action)
	(nth 1 action)
	(lambda ()
	  (interactive)
	  (eplot--execute-transient (nth 1 action)))))

(defvar eplot--transient-settings nil)

(defun eplot--execute-transient (action)
  (unless eplot--transient-settings
    (setq-local eplot--transient-settings (make-hash-table)))
  (let* ((name (intern (downcase action)))
	 (spec (assq name (append eplot--chart-headers eplot--plot-headers)))
	 (type (plist-get (cdr spec) :type)))
    ;; Sanity check.
    (unless spec
      (error "No such header type: %s" name))
    (setf (gethash name eplot--transient-settings)
	  (cond
	   ((eq type 'number)
	    (read-number (format "Value for %s (%s): " action type)))
	   ((string-match "color" (downcase action))
	    (read-color (format "Value for %s (color): " action)))
	   ((eq type 'symbol)
	    (intern
	     (completing-read (format "Value for %s: " action)
			      (plist-get (cdr spec) :valid))))
	   (t
	    (read-string (format "Value for %s (string): " action)))))
    (eplot-update-view-buffer)))
  
(eval `(transient-define-prefix eplot-customize ()
	 "Customize Chart"
	 ,@(eplot--define-transients)))

(provide 'eplot)

;;; eplot.el ends here
