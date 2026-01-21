;;; eplot.el --- Manage and Edit Wordpress Posts -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: charts
;; Package: eplot
;; Version: 1.0
;; Package-Requires: ((emacs "29.0.59") (pcsv "0.0"))

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

;; This requires the pcsv package to parse CSV files.

;;; Code:

(require 'svg)
(require 'cl-lib)
(require 'face-remap)
(require 'eieio)
(require 'iso8601)
(require 'transient)

(defvar eplot--user-defaults nil)
(defvar eplot--chart-headers nil)
(defvar eplot--plot-headers nil)
(defvar eplot--transient-settings nil)


(defvar eplot--colors
  '("aliceblue" "antiquewhite" "aqua" "aquamarine" "azure" "beige" "bisque"
    "black" "blanchedalmond" "blue" "blueviolet" "brown" "burlywood"
    "cadetblue" "chartreuse" "chocolate" "coral" "cornflowerblue" "cornsilk"
    "crimson" "cyan" "darkblue" "darkcyan" "darkgoldenrod" "darkgray"
    "darkgreen" "darkgrey" "darkkhaki" "darkmagenta" "darkolivegreen"
    "darkorange" "darkorchid" "darkred" "darksalmon" "darkseagreen"
    "darkslateblue" "darkslategray" "darkslategrey" "darkturquoise"
    "darkviolet" "deeppink" "deepskyblue" "dimgray" "dimgrey" "dodgerblue"
    "firebrick" "floralwhite" "forestgreen" "fuchsia" "gainsboro" "ghostwhite"
    "gold" "goldenrod" "gray" "green" "greenyellow" "grey" "honeydew" "hotpink"
    "indianred" "indigo" "ivory" "khaki" "lavender" "lavenderblush" "lawngreen"
    "lemonchiffon" "lightblue" "lightcoral" "lightcyan" "lightgoldenrodyellow"
    "lightgray" "lightgreen" "lightgrey" "lightpink" "lightsalmon"
    "lightseagreen" "lightskyblue" "lightslategray" "lightslategrey"
    "lightsteelblue" "lightyellow" "lime" "limegreen" "linen" "magenta"
    "maroon" "mediumaquamarine" "mediumblue" "mediumorchid" "mediumpurple"
    "mediumseagreen" "mediumslateblue" "mediumspringgreen" "mediumturquoise"
    "mediumvioletred" "midnightblue" "mintcream" "mistyrose" "moccasin"
    "navajowhite" "navy" "oldlace" "olive" "olivedrab" "orange" "orangered"
    "orchid" "palegoldenrod" "palegreen" "paleturquoise" "palevioletred"
    "papayawhip" "peachpuff" "peru" "pink" "plum" "powderblue" "purple" "red"
    "rosybrown" "royalblue" "saddlebrown" "salmon" "sandybrown" "seagreen"
    "seashell" "sienna" "silver" "skyblue" "slateblue" "slategray" "slategrey"
    "snow" "springgreen" "steelblue" "tan" "teal" "thistle" "tomato"
    "turquoise" "violet" "wheat" "white" "whitesmoke" "yellow" "yellowgreen"))

(defun eplot-set (header value)
  "Set the default value of HEADER to VALUE.
To get a list of all possible HEADERs, use the `M-x
eplot-list-chart-headers' command.

Also see `eplot-reset'."
  (let ((elem (or (assq header eplot--chart-headers)
		  (assq header eplot--plot-headers))))
    (unless elem
      (error "No such header type: %s" header))
    (eplot--add-default header value)))

(defun eplot--add-default (header value)
  ;; We want to preserve the order defaults have been added, so that
  ;; we can apply them in the same order.  This makes a difference
  ;; when we're dealing with specs that have inheritence.
  (setq eplot--user-defaults (delq (assq header eplot--user-defaults)
				   eplot--user-defaults))
  (setq eplot--user-defaults (list (cons header value))))

(defun eplot-reset (&optional header)
  "Reset HEADER to defaults.
If HEADER is nil or not present, reset everything to defaults."
  (if header
      (setq eplot--user-defaults (delq (assq header eplot--user-defaults)
				       eplot--user-defaults))
    (setq eplot--user-defaults nil)))

(unless (assoc "\\.plt" auto-mode-alist)
  (setq auto-mode-alist (cons '("\\.plt" . eplot-mode) auto-mode-alist)))

;;; eplot modes.

(defvar-keymap eplot-mode-map
  "C-c C-c" #'eplot-update-view-buffer
  "C-c C-p" #'eplot-switch-view-buffer
  "C-c C-e" #'eplot-list-chart-headers
  "C-c C-v" #'eplot-customize
  "C-c C-l" #'eplot-create-controls
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
  (or
   ;; Complete headers names.
   (and (or (looking-at ".*:")
	    (and (looking-at "[ \t]*$")
		 (save-excursion
		   (beginning-of-line)
		   (not (looking-at "\\(.+\\):")))))
	(lambda ()
	  (let ((headers (mapcar
			  (lambda (h)
			    (if (looking-at ".*:")
				(capitalize (symbol-name (car h)))
			      (concat (capitalize (symbol-name (car h))) ": ")))
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
	    'completion-attempted)))
   ;; Complete header values.
   (let ((hname nil))
     (and (save-excursion
	    (and (looking-at "[ \t]*$")
		 (progn
		   (beginning-of-line)
		   (and (looking-at "\\(.+\\):")
			(setq hname (intern (downcase (match-string 1)))))))
	    (lambda ()
	      (let ((valid (plist-get
			    (cdr (assq hname (append eplot--plot-headers
						     eplot--chart-headers)))
			    :valid))
		    (completion-ignore-case t))
		(completion-in-region
		 (save-excursion
		   (search-backward ":" (pos-bol) t)
		   (skip-chars-forward ": \t")
		   (point))
		 (line-end-position)
		 (mapcar #'symbol-name valid))
		'completion-attempted)))))))

(define-minor-mode eplot-minor-mode
  "Minor mode to issue commands from an eplot data buffer."
  :lighter " eplot")

(defvar-keymap eplot-minor-mode-map
  "H-l" #'eplot-eval-and-update)

(defvar-keymap eplot-view-mode-map
  "s" #'eplot-view-write-file
  "w" #'eplot-view-write-scaled-file
  "c" #'eplot-view-customize
  "l" #'eplot-create-controls)

(define-derived-mode eplot-view-mode special-mode "eplot view"
  "Major mode for displaying eplots."
  (setq-local revert-buffer-function #'eplot-update
	      cursor-type nil))

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

(defun eplot-view-customize ()
  "Customize the settings for the chart in the current buffer."
  (interactive)
  (with-suppressed-warnings ((interactive-only eplot-customize))
    (eplot-customize)))

(defvar eplot--data-buffer nil)
(defvar eplot--current-chart nil)

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

(defun eplot-make-plot (headers &rest datas)
  "Return an SVG based on DATA.
DATA should be pairs of headers, then followed by the plot data."
  (with-temp-buffer
    ;; Insert the headers.
    (dolist (line headers)
      (insert (format "%s:" (pop line)))
      (dolist (elem line)
	(insert (format " %s" elem)))
      (insert "\n"))
    ;; Then insert all the plot data sets.
    (dolist (data datas)
      (insert "\n")
      (dolist (line data)
	(dolist (elem line)
	  (insert (format "%s" elem) " "))
	(insert "\n")))
    ;;(setq a (buffer-string)) (debug)
    (eplot--render (eplot--parse-buffer) t)))

(defun eplot-switch-view-buffer ()
  "Switch to the eplot view buffer and render the chart."
  (interactive)
  (eplot-update-view-buffer nil t))

(defun eplot-update-view-buffer (&optional headers switch)
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
	   (transient eplot--transient-settings)
	   (data (eplot--parse-buffer))
	   (data-buffer (current-buffer))
	   (window (selected-window)))
      (unless data
	(user-error "No data in the current buffer"))
      (setq data (eplot--inject-headers data headers))
      (if (get-buffer-window "*eplot*" t)
	  (set-buffer "*eplot*")
	(if switch
	    (pop-to-buffer-same-window "*eplot*")
	  (pop-to-buffer "*eplot*")))
      (let ((inhibit-read-only t))
	(erase-buffer)
	(unless (eq major-mode 'eplot-view-mode)
	  (eplot-view-mode))
	(setq-local eplot--data-buffer data-buffer)
	(let ((eplot--transient-settings transient))
	  (let ((chart (eplot--render data)))
	    (with-current-buffer data-buffer
	      (setq-local eplot--current-chart chart))))
	(insert "\n")
	(when-let ((win (get-buffer-window "*eplot*" t)))
	  (set-window-point win (point-min))))
      (select-window window))))

(defun eplot--settings-table ()
  (if (not eplot--transient-settings)
      eplot--user-defaults
    (append eplot--user-defaults eplot--transient-settings)))

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
	(eplot--user-defaults (with-current-buffer eplot--data-buffer
				eplot--user-defaults))
	(eplot--transient-settings (with-current-buffer eplot--data-buffer
				     eplot--transient-settings))
	(inhibit-read-only t))
    (erase-buffer)
    (let ((chart (eplot--render data)))
      (with-current-buffer eplot--data-buffer
	(setq-local eplot--current-chart chart)))
    (insert "\n\n")))

(defun eplot--parse-buffer ()
  (if (eq major-mode 'org-mode)
      (eplot--parse-org-buffer)
    (eplot--parse-eplot-buffer)))

(defun eplot--parse-eplot-buffer ()
  (if (eplot--csv-buffer-p)
      (eplot--parse-csv-buffer)
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
	  data)))))

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
	 (xy (or (memq 'year data-format)
		 (memq 'date data-format)
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
	(let ((numbers (match-string 1))
	      (settings (eplot--parse-settings (match-string 3)))
	      this)
	  (setq numbers (mapcar #'string-to-number
				(split-string (string-trim numbers))))
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

(eplot-def (format symbol normal (normal bar-chart horizontal-bar-chart))
  "The overall format of the chart.")

(eplot-def (layout symbol nil (normal compact))
  "The general layout of the chart.")

(eplot-def (mode symbol light (dark light))
  "Dark/light mode.")

(eplot-def (margin-left number 70)
  "The left margin.")

(eplot-def (margin-right number 20)
  "The right margin.")

(eplot-def (margin-top number 40)
  "The top margin.")

(eplot-def (margin-bottom number 60)
  "The bottom margin.")

(eplot-def (x-axis-title-space number 5)
  "The space between the X axis and the label.")

(eplot-def (font string "sans-serif")
  "The font to use in titles, labels and legends.")

(eplot-def (font-size number 12)
  "The font size.")

(eplot-def (font-weight symbol bold (bold normal))
  "The font weight.")

(eplot-def (label-font string (spec font))
  "The font to use for axes labels.")

(eplot-def (label-font-size number (spec font-size))
  "The font size to use for axes labels.")

(eplot-def (horizontal-label-font-size number (spec label-font-size))
  "The font size to use for horizontal labels.")

(eplot-def (bar-font string (spec font))
  "The font to use for bar chart labels.")

(eplot-def (bar-font-size number (spec font-size))
  "The font size to use for bar chart labels.")

(eplot-def (bar-font-weight symbol (spec font-weight) (bold normal))
  "The font weight to use for bar chart labels.")

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
  "What grid axes to do.")

(eplot-def (grid-opacity number)
  "The opacity of the grid.
This should either be nil or a value between 0 and 1, where 0 is
fully transparent.")

(eplot-def (grid-position symbol bottom (bottom top))
  "Whether to put the grid on top or under the plot.")

(eplot-def (legend symbol nil (true nil))
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

(eplot-def (title-font number (spec font))
  "The font size to use for axes labels.")

(eplot-def (title-font-size number (spec font-size))
  "The font size to use for axes labels.")

(eplot-def (x-title string)
  "The title of the X axis, if any.")

(eplot-def (y-title string)
  "The title of the X axis, if any.")

(eplot-def (x-label-format string)
  "Format string for the X labels.
This is a `format' string.")

(eplot-def (y-label-format string)
  "Format string for the Y labels.
This is a `format' string.")

(eplot-def (horizontal-label-left number)
  "Position of the horizontal labels.")

(eplot-def (x-label-orientation symbol horizontal (horizontal vertical))
  "Orientation of the X labels.")

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
    (x-axis-title-space 3)))

(defvar eplot-dark-defaults
  '((chart-color "#c0c0c0")
    (axes-color "#c0c0c0")
    (grid-color "#404040")
    (background-color "#101010")
    (label-color "#c0c0c0")
    (legend-color "#c0c0c0")
    (legend-background-color "#000000")
    (title-color "#c0c0c0")))

(defvar eplot-bar-chart-defaults
  '((grid-position top)
    (grid y)
    (grid-opacity 0.2)
    (grid-color "#a0a0a0")
    (min 0)))

(defvar eplot-horizontal-bar-chart-defaults
  '((grid-position top)
    (grid-opacity 0.2)
    (min 0)))

(defclass eplot-chart ()
  (
   (plots :initarg :plots)
   (data :initarg :data)
   (xs)
   (ys)
   (x-values :initform nil)
   (x-type :initform nil)
   (x-min)
   (x-max)
   (x-ticks)
   (y-ticks)
   (y-labels)
   (x-labels)
   (print-format)
   (x-tick-step)
   (x-label-step)
   (x-step-map :initform nil)
   (y-tick-step)
   (y-label-step)
   (inhibit-compute-x-step :initform nil)
   ;; ---- CUT HERE ----
   (axes-color :initarg :axes-color :initform nil)
   (background-color :initarg :background-color :initform nil)
   (background-gradient :initarg :background-gradient :initform nil)
   (background-image-cover :initarg :background-image-cover :initform nil)
   (background-image-file :initarg :background-image-file :initform nil)
   (background-image-opacity :initarg :background-image-opacity :initform nil)
   (bar-font :initarg :bar-font :initform nil)
   (bar-font-size :initarg :bar-font-size :initform nil)
   (bar-font-weight :initarg :bar-font-weight :initform nil)
   (border-color :initarg :border-color :initform nil)
   (border-width :initarg :border-width :initform nil)
   (chart-color :initarg :chart-color :initform nil)
   (font :initarg :font :initform nil)
   (font-size :initarg :font-size :initform nil)
   (font-weight :initarg :font-weight :initform nil)
   (format :initarg :format :initform nil)
   (frame-color :initarg :frame-color :initform nil)
   (frame-width :initarg :frame-width :initform nil)
   (grid :initarg :grid :initform nil)
   (grid-color :initarg :grid-color :initform nil)
   (grid-opacity :initarg :grid-opacity :initform nil)
   (grid-position :initarg :grid-position :initform nil)
   (header-file :initarg :header-file :initform nil)
   (height :initarg :height :initform nil)
   (horizontal-label-font-size :initarg :horizontal-label-font-size :initform nil)
   (label-color :initarg :label-color :initform nil)
   (label-font :initarg :label-font :initform nil)
   (label-font-size :initarg :label-font-size :initform nil)
   (layout :initarg :layout :initform nil)
   (legend :initarg :legend :initform nil)
   (legend-background-color :initarg :legend-background-color :initform nil)
   (legend-border-color :initarg :legend-border-color :initform nil)
   (legend-color :initarg :legend-color :initform nil)
   (margin-bottom :initarg :margin-bottom :initform nil)
   (margin-left :initarg :margin-left :initform nil)
   (margin-right :initarg :margin-right :initform nil)
   (margin-top :initarg :margin-top :initform nil)
   (max :initarg :max :initform nil)
   (min :initarg :min :initform nil)
   (mode :initarg :mode :initform nil)
   (surround-color :initarg :surround-color :initform nil)
   (title :initarg :title :initform nil)
   (title-font :initarg :title-font :initform nil)
   (title-font-size :initarg :title-font-size :initform nil)
   (title-color :initarg :title-color :initform nil)
   (width :initarg :width :initform nil)
   (x-axis-title-space :initarg :x-axis-title-space :initform nil)
   (x-title :initarg :x-title :initform nil)
   (y-title :initarg :y-title :initform nil)
   (x-label-format :initarg :x-label-format :initform nil)
   (x-label-orientation :initarg :x-label-orientation :initform nil)
   (y-label-format :initarg :y-label-format :initform nil)
   (horizontal-label-left :initarg :horizontal-label-left :initform nil)
   ;; ---- CUT HERE ----
   ))

;;; Parameters that are plot specific.

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

(eplot-pdef (style symbol line ( line impulse point square circle cross
				 triangle rectangle curve))
  "Style the plot should be drawn in.
Valid values are listed below.  Some styles take additional
optional parameters.

line
  Straight lines between values.

curve
  Curved lines between values.

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

(eplot-pdef (size-factor number)
  "Multiply the size of the elements by the value.")

(eplot-pdef (data-file string)
  "File where the data is.")

(eplot-pdef (data-format symbol-list nil (nil two-values date time))
  "List of symbols to describe the data format.
Elements allowed are `two-values', `date' and `time'.")

(eplot-pdef (name string)
  "Name of the plot, which will be displayed if legends are switched on.")

(eplot-pdef (legend-color string (spec chart-color))
  "Color for the name to be displayed in the legend.")

(eplot-pdef (bezier-factor number 0.1)
  "The Bezier factor to apply to curve plots.")

(eplot-pdef (bar-max-width number)
  "Max width of bars in bar plots.")

(defclass eplot-plot ()
  (
   (values :initarg :values)
   ;; ---- CUT HERE ----
   (bar-max-width :initarg :bar-max-width :initform nil)
   (bezier-factor :initarg :bezier-factor :initform nil)
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
   (size-factor :initarg :size-factor :initform nil)
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
    ;; One special case.  I don't think this hack is quite right...
    (when (or (eq (eplot--vs 'mode data) 'dark)
	      (eq (cdr (assq 'mode eplot--user-defaults)) 'dark))
      (setf (slot-value plot 'color) "#c0c0c0"))
    ;; Use the headers.
    (eplot--object-values plot (cdr (assq :headers data)) eplot--plot-headers)
    ;; Finally, set defaults from user settings/transients.
    (cl-loop for (name . value) in eplot--transient-settings
	     when (assq name eplot--plot-headers)
	     do
	     (setf (slot-value plot name) value)
	     (eplot--set-dependent-values plot name value))
    plot))

(defun eplot--make-chart (data)
  "Make an `eplot-chart' object and initialize based on DATA."
  (let ((chart (make-instance 'eplot-chart
			      :plots (mapcar #'eplot--make-plot
					     (eplot--vs :plots data))
			      :data data)))
    ;; First get the program-defined defaults.
    (eplot--object-defaults chart eplot--chart-headers)
    ;; Then do the "meta" variables.
    (eplot--meta chart data 'mode 'dark eplot-dark-defaults)
    (eplot--meta chart data 'layout 'compact eplot-compact-defaults)
    (eplot--meta chart data 'format 'bar-chart eplot-bar-chart-defaults)
    (eplot--meta chart data 'format 'horizontal-bar-chart
		 eplot-horizontal-bar-chart-defaults)
    ;; Set defaults from user settings.
    (cl-loop for (name . value) in eplot--user-defaults
	     when (assq name eplot--chart-headers)
	     do
	     (setf (slot-value chart name) value)
	     (eplot--set-dependent-values chart name value))
    ;; Use the data from the chart.
    (eplot--object-values chart data eplot--chart-headers)
    ;; Finally, set defaults from user settings/transients.
    (cl-loop for (name . value) in eplot--transient-settings
	     when (assq name eplot--chart-headers)
	     do
	     (setf (slot-value chart name) value)
	     (eplot--set-dependent-values chart name value))
    ;; If not set, recompute the margins based on the font sizes (if
    ;; the font size has been changed from defaults).
    (when (or (assq 'font-size eplot--user-defaults)
	      (assq 'font-size data))
      (with-slots ( title x-title y-title
		    margin-top margin-bottom margin-left
		    font-size font font-weight)
	  chart
	(when (or title x-title y-title)
	  (let ((text-height
		 (eplot--text-height (concat title x-title y-title)
				     font font-size font-weight)))
	    (when (and title
		       (and (not (assq 'margin-top eplot--user-defaults))
			    (not (assq 'margin-top data))))
	      (cl-incf margin-top (* text-height 1.4)))
	    (when (and x-title
		       (and (not (assq 'margin-bottom eplot--user-defaults))
			    (not (assq 'margin-bottom data))))
	      (cl-incf margin-bottom (* text-height 1.4)))
	    (when (and y-title
		       (and (not (assq 'margin-left eplot--user-defaults))
			    (not (assq 'margin-left data))))
	      (cl-incf margin-left (* text-height 1.4)))))))
    chart))

(defun eplot--meta (chart data slot value defaults)
  (when (or (eq (cdr (assq slot eplot--user-defaults)) value)
	    (eq (eplot--vy slot data) value))
    (eplot--set-theme chart defaults)))

(defun eplot--object-defaults (object headers)
  (dolist (header headers)
    (when-let ((default (plist-get (cdr header) :default)))
      (setf (slot-value object (car header))
	    ;; Allow overrides via `eplot-set'.
	    (or (cdr (assq (car header) eplot--user-defaults))
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
	(or (cdr (assq slot eplot--user-defaults)) default)))))

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
		  grid-position plots x-min format
		  x-label-orientation)
	chart
      ;; Set the size of the chart based on the window it's going to
      ;; be displayed in.  It uses the *eplot* window by default, or
      ;; the current one if that isn't displayed.
      (let ((factor (image-compute-scaling-factor image-scaling-factor)))
	(unless width
	  (setq width (truncate
		       (/ (* (window-pixel-width
 			      (get-buffer-window "*eplot*" t))
			     0.9)
			  factor))))
	(unless height
	  (setq height (truncate
			(/ (* (window-pixel-height
			       (get-buffer-window "*eplot*" t))
			      0.9)
			   factor)))))
      (setq svg (svg-create width height)
	    xs (- width margin-left margin-right)
	    ys (- height margin-top margin-bottom))
      ;; Protect against being called in an empty buffer.
      (if (not (and plots
		    ;; Sanity check against the user choosing dimensions
		    ;; that leave no space for the plot.
		    (> ys 0) (> xs 0)))
	  ;; Just draw the basics.
	  (eplot--draw-basics svg chart)

	;; Horizontal bar charts are special.
	(when (eq format 'horizontal-bar-chart)
	  (eplot--adjust-horizontal-bar-chart chart data))
	;; Compute min/max based on all plots, and also compute x-ticks
	;; etc.
	(eplot--compute-chart-dimensions chart)
	(when (and (eq x-label-orientation 'vertical)
		   (eplot--default-p 'margin-bottom (slot-value chart 'data)))
	  (eplot--adjust-vertical-x-labels chart))
	;; Analyze values and adjust values accordingly.
	(eplot--adjust-chart chart)
	;; Compute the Y labels -- this may adjust `margin-left'.
	(eplot--compute-y-labels chart)
	;; Compute the X labels -- this may adjust `margin-bottom'.
	(eplot--compute-x-labels chart)
	;; Draw background/borders/titles/etc.
	(eplot--draw-basics svg chart)

	(when (eq grid-position 'top)
	  (eplot--draw-plots svg chart))

	(eplot--draw-x-ticks svg chart)
	(unless (eq format 'horizontal-bar-chart)
	  (eplot--draw-y-ticks svg chart))
      
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
      (svg-insert-image svg)
      chart)))

(defun eplot--adjust-horizontal-bar-chart (chart data)
  (with-slots ( plots bar-font bar-font-size bar-font-weight margin-left
		width margin-right xs)
      chart
    (dolist (plot plots)
      (with-slots ( data-format values) plot
	(push 'xy data-format)
	;; Flip the values -- we want the values to be on the X
	;; axis instead.
	(setf values
	      (cl-loop for value in values
		       for i from 1
		       collect (list :value i
				     :x (plist-get value :value)
				     :settings
				     (plist-get value :settings))))
	(when (eplot--default-p 'margin-left data)
	  (setf margin-left
		(+ (cl-loop for value in values
			    maximize
			    (eplot--text-width
			     (eplot--vs 'label (plist-get value :settings))
			     bar-font bar-font-size bar-font-weight))
		   20)
		xs (- width margin-left margin-right)))))))

(defun eplot--draw-basics (svg chart)
  (with-slots ( width height 
		chart-color font font-size font-weight
		margin-left margin-right margin-top margin-bottom
		background-color label-color
		xs ys)
      chart
    ;; Add background.
    (eplot--draw-background chart svg 0 0 width height)
    (with-slots ( background-image-file background-image-opacity
		  background-image-cover)
	chart
      (when (and background-image-file
		 ;; Sanity checks to avoid erroring out later.
		 (file-exists-p background-image-file)
		 (file-regular-p background-image-file))
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
    (with-slots (title title-font title-font-size title-color) chart
      (when title
	(svg-text svg title
		  :font-family title-font
		  :text-anchor "middle"
		  :font-weight font-weight
		  :font-size title-font-size
		  :fill title-color
		  :x (+ margin-left (/ (- width margin-left margin-right) 2))
		  :y (+ 3 (/ margin-top 2)))))
    (with-slots (x-title) chart
      (when x-title
	(let ((theight (eplot--text-height
			x-title font font-size font-weight)))
	  (svg-text svg x-title
		    :font-family font
		    :text-anchor "middle"
		    :font-weight font-weight
		    :font-size font-size
		    :fill label-color
		    :x (+ margin-left (/ (- width margin-left margin-right) 2))
		    ;; The bottom margin has room for the x ticks at the top.
		    ;; Put the title in the middle of the area left after
		    ;; subtracting that space.
		    :y (- height (/ (- (- margin-bottom theight)
				       theight)
				    2))))))
    (with-slots (y-title) chart
      (when y-title
	(let ((text-height
	       (eplot--text-height y-title font font-size font-weight)))
	  (svg-text svg y-title
		    :font-family font
		    :text-anchor "middle"
		    :font-weight font-weight
		    :font-size font-size
		    :fill label-color
		    :transform
		    (format "translate(%s,%s) rotate(-90)"
			    (- (/ margin-left 2) (/ text-height 2) 4)
			    (+ margin-top
			       (/ (- height margin-bottom margin-top) 2)))))))))

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
  (with-slots ( min max plots x-values x-min x-max x-ticks
		print-format font-size
		xs
		inhibit-compute-x-step x-type x-step-map format
		x-tick-step x-label-step
		label-font label-font-size x-label-format)
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
				  ((memq 'year data-format) 'year)
				  ((memq 'date data-format) 'date)
				  ((memq 'time data-format) 'time)
				  (t 'number)))
	      (cond
	       ((or (memq 'xy data-format)
		    (memq 'year data-format))
		(setq x-values (cl-loop for val in values
					collect (plist-get val :x))
		      x-min (if (eq format 'horizontal-bar-chart)
				0
			      (seq-min x-values))
		      x-max (seq-max x-values)
		      x-ticks (eplot--get-ticks x-min x-max xs))
		(when (memq 'year data-format)
		  (setq print-format 'literal-year)))
	       ((memq 'date data-format)
		(setq x-values
		      (cl-loop for val in values
			       collect
			       (time-to-days
				(encode-time
				 (decoded-time-set-defaults
				  (iso8601-parse-date
				   (format "%d" (plist-get val :x)))))))
		      x-min (seq-min x-values)
		      x-max (seq-max x-values)
		      inhibit-compute-x-step t)
		(let ((xs (eplot--get-date-ticks
			   x-min x-max xs
			   label-font label-font-size x-label-format)))
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
		      inhibit-compute-x-step t)
		(let ((xs (eplot--get-time-ticks
			   x-min x-max xs label-font label-font-size
			   x-label-format)))
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
		y-ticks xs x-values print-format
		x-label-format label-font label-font-size data
		x-ticks)
      chart
    (setq y-ticks (and max
		       (eplot--get-ticks
			min
			;; We get 5% more ticks to check whether we
			;; should extend max.
			(if (eplot--default-p 'max data)
			    (* max 1.05)
			  max)
			ys)))
    (when (eplot--default-p 'max data)
      (setq max (max max (car (last y-ticks)))))
    (if (eq format 'bar-chart)
	(setq x-tick-step 1
	      x-label-step 1)
      (unless inhibit-compute-x-step
	(let ((xt (eplot--compute-x-ticks
		   xs x-ticks print-format
		   x-label-format label-font label-font-size)))
	  (setq x-tick-step (car xt)
		x-label-step (cadr xt)))))
    (when max
      (let ((yt (eplot--compute-y-ticks
		 ys y-ticks
		 (eplot--text-height "100" label-font label-font-size))))
	(setq y-tick-step (car yt)
	      y-label-step (cadr yt))))
    ;; If max is less than 2% off from a pleasant number, then
    ;; increase max.
    (when (eplot--default-p 'max data)
      (cl-loop for tick in (reverse y-ticks)
	       when (and (< max tick)
			 (< (e/ (- tick max) (- max min)) 0.02))
	       return (progn
			(setq max tick)
			;; Chop off any further ticks.
			(setcdr (member tick y-ticks) nil))))

    (when y-ticks
      (if (and (eplot--default-p 'min data)
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

(defun eplot--adjust-vertical-x-labels (chart)
  (with-slots ( x-step-map x-ticks format plots
		print-format x-label-format label-font
		label-font-size margin-bottom
		bar-font bar-font-size bar-font-weight)
      chart
    ;; Make X ticks.
    (let ((width
	   (cl-loop
	    for xv in (or x-step-map x-ticks)
	    for x = (if (consp xv) (car xv) xv)
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
			  (eplot--format-value x print-format x-label-format))
	    maximize (if (equal format 'bar-chart)
			 (eplot--text-width
			  label bar-font bar-font-size bar-font-weight)
		       (eplot--text-width
			label label-font label-font-size)))))
      ;; Ensure that we have enough room to display the X labels
      ;; (unless overridden).
      (with-slots ( height margin-top ys
		    y-ticks y-tick-step y-label-step min max)
	  chart
	(setq margin-bottom (max margin-bottom (+ width 40))
	      ys (- height margin-top margin-bottom))))))

(defun eplot--compute-x-labels (chart)
  (with-slots ( x-step-map x-ticks
		format plots print-format x-label-format x-labels
		x-tick-step x-label-step
		x-label-orientation margin-bottom)
      chart
    ;; Make X ticks.
    (setf x-labels
	  (cl-loop
	   for xv in (or x-step-map x-ticks)
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
	   collect (list
		    (if (equal format 'bar-chart)
			(eplot--vs 'label
				   (plist-get value :settings)
				   ;; When we're doing bar charts, we
				   ;; want default labeling to start with
				   ;; 1 and not zero.
				   (format "%s" (1+ x)))
		      (eplot--format-value x print-format x-label-format))
		    do-tick
		    do-label)))))

(defun eplot--draw-x-ticks (svg chart)
  (with-slots ( x-step-map x-ticks format layout print-format
		margin-left margin-right margin-top margin-bottom
		x-min x-max xs
		width height
		axes-color label-color
		grid grid-opacity grid-color
		font x-tick-step x-label-step x-label-format x-label-orientation
		label-font label-font-size
		plots x-labels
		x-values
		bar-font bar-font-size bar-font-weight
		plots)
      chart
    (let ((font label-font)
	  (font-size label-font-size)
	  (font-weight 'normal)
	  (label-settings nil))
      (when (equal format 'bar-chart)
	(setq font bar-font
	      font-size bar-font-size
	      font-weight bar-font-weight
	      label-settings (mapcar (lambda (e)
				       (plist-get e :settings))
				     (slot-value (car plots) 'values))))
      ;; Make X ticks.
      (cl-loop with label-height
	       for xv in (or x-step-map x-ticks)
	       for x = (if (consp xv) (car xv) xv)
	       for i from 0
	       for (label do-tick do-label) in x-labels
	       for px = (if (equal format 'bar-chart)
			    (+ margin-left
			       ;; Adjust for extra bar-width to the left.
			       (/ (* (/ xs (length x-values)) 1.1) 2)
			       (* (e/ i (length x-values))
				  xs))
			  (+ margin-left
			     (* (/ (- (* 1.0 x) x-min) (- x-max x-min))
				xs)))
	       for this-font-weight =
	       (if (equal format 'bar-chart)
		   (or (cdr (assq 'label-font-weight (nth i label-settings)))
		       font-weight)
		 font-weight)
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
		 (if (eq x-label-orientation 'vertical)
		     (progn
		       (unless label-height
			 ;; The X position we're putting the label at is
			 ;; based on the bottom of the lower-case
			 ;; characters.  So we want to ignore descenders
			 ;; etc, so we use "xx" to determine the height
			 ;; to be able to center the text.
			 (setq label-height
			       (eplot--text-height
				;; If the labels are numerical, we need
				;; to center them using the height of
				;; numbers.
				(if (string-match "^[0-9]+$" label)
				    "10"
				  ;; Otherwise center them on the baseline.
				  "xx")
				font font-size this-font-weight)))
		       (svg-text svg label
				 :font-family font
				 :text-anchor "end"
				 :font-size font-size
				 :font-weight this-font-weight
				 :fill label-color
				 :transform
				 (format "translate(%s,%s) rotate(-90)"
					 (+ px (/ label-height 2))
					 (- height margin-bottom -10))))
		   (svg-text svg label
			     :font-family font
			     :text-anchor "middle"
			     :font-size font-size
			     :font-weight this-font-weight
			     :fill label-color
			     :x px
			     :y (+ (- height margin-bottom)
				   font-size
				   (if (equal format 'bar-chart)
				       (if (equal layout 'compact) 3 5)
				     2)))))))))

(defun eplot--stride (chart values)
  (with-slots (xs x-type format) chart
    (if (eq x-type 'one-dimensional)
	(e/ xs
	    ;; Fenceposting bar-chart vs everything else.
	    (if (eq format 'bar-chart)
		(length values)
	      (1- (length values))))
      (e/ xs (length values)))))

(defun eplot--default-p (slot data)
  "Return non-nil if SLOT is at the default value."
  (and (not (assq slot eplot--user-defaults))
       (not (assq slot data))))

(defun eplot--compute-y-labels (chart)
  (with-slots ( y-ticks y-labels
		width height min max xs ys
		margin-top margin-bottom margin-left margin-right
		y-tick-step y-label-step y-label-format)
      chart
    ;; First collect all the labels we're thinking about outputting.
    (setq y-labels
	  (cl-loop for y in y-ticks
		   for py = (- (- height margin-bottom)
			       (* (/ (- (* 1.0 y) min) (- max min))
				  ys))
		   when (and (<= margin-top py (- height margin-bottom))
			     (zerop (e% y y-tick-step))
			     (zerop (e% y y-label-step)))
		   collect (eplot--format-y
			    y (- (cadr y-ticks) (car y-ticks)) nil
			    y-label-format)))
    ;; Check the labels to see whether we have too many digits for
    ;; what we're actually going to display.  Man, this is a lot of
    ;; back-and-forth and should be rewritten to be less insanely
    ;; inefficient.
    (when (= (seq-count (lambda (label)
			  (string-match "\\." label))
			y-labels)
	     (length y-labels))
      (setq y-labels
	    (cl-loop with max = (cl-loop for label in y-labels
					 maximize (eplot--decimal-digits
						   (string-to-number label)))
		     for label in y-labels
		     collect (format (if (zerop max)
					 "%d"
				       (format "%%.%df" max))
				     (string-to-number label)))))
    (setq y-labels (cl-coerce y-labels 'vector))
    ;; Ensure that we have enough room to display the Y labels
    ;; (unless overridden).
    (when (eplot--default-p 'margin-left (slot-value chart 'data))
      (with-slots (label-font label-font-size) chart
	(setq margin-left (max margin-left
			       (+ (eplot--text-width
				   (elt y-labels (1- (length y-labels)))
				   label-font label-font-size)
				  10))
	      xs (- width margin-left margin-right))))))

(defun eplot--draw-y-ticks (svg chart)
  (with-slots ( y-ticks y-labels y-tick-step y-label-step label-color
		label-font label-font-size
		width height min max ys
		margin-top margin-bottom margin-left margin-right
		axes-color
		grid grid-opacity grid-color)
      chart
    ;; Make Y ticks.
    (cl-loop with lnum = 0
	     with text-height = (eplot--text-height
				 "012" label-font label-font-size)
	     for y in y-ticks
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
		 (svg-text svg (elt y-labels lnum)
			   :font-family label-font
			   :text-anchor "end"
			   :font-size label-font-size
			   :fill label-color
			   :x (- margin-left 6)
			   :y (+ py (/ text-height 2) -1))
		 (cl-incf lnum))))))

(defun eplot--text-width (text font font-size &optional font-weight)
  (string-pixel-width
   (propertize text 'face
	       (list :font (font-spec :family font
				      :weight (or font-weight 'normal)
				      :size font-size)))))

(defvar eplot--text-size-cache (make-hash-table :test #'equal))

(defun eplot--text-height (text font font-size &optional font-weight)
  (cdr (eplot--text-size text font font-size font-weight)))

(defun eplot--text-size (text font font-size font-weight)
  (let ((key (list text font font-size font-weight)))
    (or (gethash key eplot--text-size-cache)
	(let ((size (eplot--text-size-1 text font font-size font-weight)))
	  (setf (gethash key eplot--text-size-cache) size)
	  size))))

(defun eplot--text-size-1 (text font font-size font-weight)
  (if (not (executable-find "convert"))
      ;; This "default" text size is kinda bogus.
      (cons (* (length text) font-size) font-size)
    (let* ((size (* font-size 10))
	   (svg (svg-create size size))
	   text-size)
      (svg-rectangle svg 0 0 size size :fill "black")
      (svg-text svg text
		:font-family font
		:text-anchor "middle"
		:font-size font-size
		:font-weight (or font-weight 'normal)
		:fill "white"
		:x (/ size 2)
		:y (/ size 2))
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(svg-print svg)
	(let* ((file (make-temp-file "eplot" nil ".svg"))
	       (png (file-name-with-extension file ".png")))
	  (unwind-protect
	      (progn
		(write-region (point-min) (point-max) file nil 'silent)
		;; rsvg-convert is 5x faster than convert when doing SVG, so
		;; if we have it, we use it.
		(when (executable-find "rsvg-convert")
		  (unwind-protect
		      (call-process "rsvg-convert" nil nil nil
				    (format "--output=%s" png) file)
		    (when (file-exists-p png)
		      (delete-file file)
		      (setq file png))))
		(erase-buffer)
		(when (zerop (call-process "convert" nil t nil
					   "-trim" "+repage" file "info:-"))
		  (goto-char (point-min))
		  (when (re-search-forward " \\([0-9]+\\)x\\([0-9]+\\)" nil t)
		    (setq text-size
			  (cons (string-to-number (match-string 1))
				(string-to-number (match-string 2)))))))
	    (when (file-exists-p file)
	      (delete-file file)))))
      (or text-size
	  ;; This "default" text size is kinda bogus.
	  (cons (* (length text) font-size) font-size)))))

(defun eplot--draw-legend (svg chart)
  (with-slots ( legend plots
		margin-left margin-top
		font font-size font-weight
		background-color axes-color
		legend-color legend-background-color legend-border-color)
      chart
    (when (eq legend 'true)
      (when-let ((names
		  (cl-loop for plot in plots
			   for name = (slot-value plot 'name)
			   when name
			   collect
			   (cons name (slot-value plot 'color)))))
	(svg-rectangle svg (+ margin-left 20) (+ margin-top 20)
		       (format "%dpx"
			       (+ 10
				  (seq-max
				   (mapcar (lambda (name)
					     (eplot--text-width (car name)
								font font-size))
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
			      :font-weight font-weight
			      :fill (or (cdr name) legend-color)
			      :x (+ margin-left 25)
			      :y (+ margin-top 40 (* i font-size))))))))

(defun eplot--format-y (y spacing whole format-string)
  (format (or format-string "%s")
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
	    (format "%s" y)))))

(defun eplot--format-value (value print-format label-format)
  (replace-regexp-in-string
   ;; Texts in SVG collapse multiple spaces into one.  So do it here,
   ;; too, so that width calculations are correct.
   " +" " "
   (cond
    ((eq print-format 'date)
     (format-time-string
      (or label-format "%Y-%m-%d") (eplot--days-to-time value)))
    ((eq print-format 'year)
     (format-time-string (or label-format "%Y") (eplot--days-to-time value)))
    ((eq print-format 'time)
     (format-time-string (or label-format "%H:%M:%S") value))
    ((eq print-format 'minute)
     (format-time-string (or label-format "%H:%M") value))
    ((eq print-format 'hour)
     (format-time-string (or label-format "%H") value))
    (t
     (format (or label-format "%s") value)))))

(defun eplot--compute-x-ticks (xs x-values print-format x-label-format
				  label-font label-font-size)
  (let* ((min (seq-min x-values))
	 (max (seq-max x-values))
	 (count (length x-values))
	 (max-print (eplot--format-value max print-format x-label-format))
	 ;; We want each label to be spaced at least as long apart as
	 ;; the length of the longest label, with room for two blanks
	 ;; in between.
	 (min-spacing (* 1.2 (eplot--text-width max-print label-font
						label-font-size)))
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

(defun eplot--compute-y-ticks (ys y-values text-height)
  (let* ((min (car y-values))
	 (max (car (last y-values)))
	 (count (length y-values))
	 ;; We want each label to be spaced at least as long apart as
	 ;; the height of the label.
	 (min-spacing (+ text-height 10))
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
		(while (> (/ (- max min) label-step) (/ ys min-spacing))
		  (setq label-step (eplot--next-weed label-step)))
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

(defun eplot--pv (plot slot &optional default)
  (let ((user (cdr (assq slot eplot--user-defaults))))
    (when (and (stringp user) (zerop (length user)))
      (setq user nil))
    (or user (slot-value plot slot) default)))

(defun eplot--draw-plots (svg chart)
  (if (eq (slot-value chart 'format) 'horizontal-bar-chart)
      (eplot--draw-horizontal-bar-chart svg chart)
    (eplot--draw-normal-plots svg chart)))
    
(defun eplot--draw-normal-plots (svg chart)
  (with-slots ( plots chart-color height format
		margin-bottom margin-left
		min max xs ys
		margin-top
		x-values x-min x-max
		label-font label-font-size)
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
	     for gradient = (eplot--parse-gradient (eplot--pv plot 'gradient))
	     for lpy = nil
	     for lpx = nil
	     for style = (if (eq format 'bar-chart)
			     'bar
			   (slot-value plot 'style))
	     for bar-max-width = (eplot--pv plot 'bar-max-width)
	     for bar-width = (and (eq style 'bar)
				  (min (or bar-max-width most-positive-fixnum)
				       (/ xs (length x-values))))
	     for bar-gap = (and (eq style 'bar)
				(if (< bar-width
				       (or bar-max-width most-positive-fixnum))
				    (* bar-width 0.1)
				  0))
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
			      (/ bar-gap 2)
			      (/ (/ xs (length x-values)) 2)
			      (* (e/ i (length x-values))
				 xs))
			 (+ margin-left
			    (* (e/ (- x x-min) (- x-max x-min))
			       xs)))
	      do
	      ;; Some data points may have texts.
	      (when-let ((text (eplot--vs 'text settings)))
		(svg-text svg text
			  :font-family label-font
			  :text-anchor "middle"
			  :font-size label-font-size
			  :font-weight 'normal
			  :fill color
			  :x px 
			  :y (- py (eplot--text-height
				    text label-font label-font-size)
				-5)))
	      ;; You may mark certain points.
	      (when-let ((mark (eplot--vy 'mark settings)))
		(cl-case mark
		  (cross
		   (let ((s (eplot--element-size val plot settings 3)))
		     (svg-line svg (- px s) (- py s)
			       (+ px s) (+ py s)
			       :clip-path clip-id
			       :stroke color)
		     (svg-line svg (+ px s) (- py s)
			       (- px s) (+ py s)
			       :clip-path clip-id
			       :stroke color)))
		  (otherwise
		   (svg-circle svg px py 3
			       :fill color))))
	      (cl-case style
		(bar
		 (if (not gradient)
		     (svg-rectangle
		      svg
		      (+ (- px (e/ bar-width 2)) (e/ bar-gap 2))
		      py
		      (- bar-width bar-gap)
		      (- height margin-bottom py)
		      :clip-path clip-id
		      :fill color)
		   (let ((id (format "gradient-%s" (make-temp-name "grad"))))
		     (eplot--gradient svg id 'linear
				      (eplot--stops (eplot--vs 'from gradient)
						    (eplot--vs 'to gradient))
				      (eplot--vs 'direction gradient))
		     (svg-rectangle
		      svg
		      (+ (- px (e/ bar-width 2)) (e/ bar-gap 2))
		      py
		      (- bar-width bar-gap)
		      (- height margin-bottom py)
		      :clip-path clip-id
		      :gradient id))))
		(impulse
		 (let ((width (eplot--element-size val plot settings 1)))
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
			       :stroke-width (eplot--pv plot 'size 1)
			       :clip-path clip-id
			       :stroke color))))
		(curve
		 (push (cons px py) polygon))
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
			     (eplot--element-size val plot settings 3)
			     :clip-path clip-id
			     :stroke color
			     :fill (eplot--vary-color
				    (eplot--vs
				     'fill-color settings
				     (or (slot-value plot 'fill-color) "none"))
				    i)))
		(cross
		 (let ((s (eplot--element-size val plot settings 3)))
		   (svg-line svg (- px s) (- py s)
			     (+ px s) (+ py s)
			     :clip-path clip-id
			     :stroke color)
		   (svg-line svg (+ px s) (- py s)
			     (- px s) (+ py s)
			     :clip-path clip-id
			     :stroke color)))
		(triangle
		 (let ((s (eplot--element-size val plot settings 5)))
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
		 (let ((s (eplot--element-size val plot settings 3)))
		   (svg-rectangle svg (- px (e/ s 2)) (- py (e/ s 2))
				  s s
				  :clip-path clip-id
				  :stroke color
				  :fill-color
				  (or (slot-value plot 'fill-color) "none")))))
	      (setq lpy py
		    lpx px))

	     ;; We're doing a gradient of some kind (or a curve), so
	     ;; draw it now when we've collected the polygon.
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
	       (when gradient
		 (if (eq (eplot--vs 'position gradient) 'above)
		     (push (cons lpx margin-top) polygon)
		   (push (cons lpx (- height margin-bottom)) polygon)))
	       (let ((id (format "gradient-%d" plot-number)))
		 (when gradient
		   (eplot--gradient svg id 'linear
				    (eplot--stops (eplot--vs 'from gradient)
						  (eplot--vs 'to gradient))
				    (eplot--vs 'direction gradient)))
		 (if (eq style 'curve)
		     (apply #'svg-path svg
			    (nconc
			     (cl-loop
			      with points = (cl-coerce
					     (nreverse polygon) 'vector)
			      for i from 0 upto (1- (length points))
			      collect
			      (cond
			       ((zerop i)
				`(moveto ((,(car (elt points 0)) .
					   ,(cdr (elt points 0))))))
			       (t
				`(curveto
				  (,(eplot--bezier
				     (eplot--pv plot 'bezier-factor)
				     i points))))))
			     (and gradient '((closepath))))
			    `( :clip-path ,clip-id
			       :stroke-width ,(eplot--pv plot 'size 1)
			       :stroke ,(slot-value plot 'color)
			       ,@(if gradient
				     `(:gradient ,id)
				   `(:fill "none"))))
		   (svg-polygon
		    svg (nreverse polygon)
		    :clip-path clip-id
		    :gradient id
		    :stroke (slot-value plot 'fill-border-color))))))))

(defun eplot--element-size (value plot settings default)
  (eplot--vn 'size settings
	     (if (slot-value plot 'size-factor)
		 (* value (slot-value plot 'size-factor))
	       (or (slot-value plot 'size) default))))

(defun eplot--draw-horizontal-bar-chart (svg chart)
  (with-slots ( plots chart-color height format
		margin-bottom margin-left
		min max xs ys
		margin-top
		x-values x-min x-max
		label-font label-font-size label-color
		horizontal-label-left horizontal-label-font-size)
      chart
    (dolist (plot plots)
      (cl-loop with values = (slot-value plot 'values)
	       with stride = (e/ ys (length values))
	       with label-height = (eplot--text-height "xx" label-font
						       label-font-size)
	       with bar-gap = (* stride 0.1)
	       for i from 0
	       for value in values
	       for settings = (plist-get value :settings)
	       for bar-max-width = (eplot--pv plot 'bar-max-width)
	       for py = (+ margin-top (* i stride))
	       for px = (* (e/ (plist-get value :x) x-max) xs)
	       for color = (eplot--vary-color
			    (eplot--vs 'color settings (slot-value plot 'color))
			    i)
	       do
	       (svg-rectangle svg
			      margin-left
			      (+ py (e/ bar-gap 2)
				 (if (and bar-max-width
					  (< bar-max-width (- stride bar-gap)))
				     (- (/ stride 2) (/ bar-max-width 2))
				   0))
			      px
			      (if bar-max-width
				  (min bar-max-width (- stride bar-gap))
				(- stride bar-gap))
			      :fill color)
	       (svg-text svg (eplot--vs 'label settings)
			 :font-family label-font
			 :text-anchor "left"
			 :font-size horizontal-label-font-size
			 :font-weight 'normal
			 :fill label-color
			 :x (or horizontal-label-left 5)
			 :y (+ py label-height
			       (/ stride 2)))))))

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

(defun eplot--days-to-time (days)
  (days-to-time (- days (time-to-days 0))))

(defun eplot--get-date-ticks (start end xs label-font label-font-size
				    x-label-format &optional skip-until)
  (let* ((duration (- end start))
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
    (let* ((x-ticks (cl-loop for day from start upto end
			     for time = (eplot--days-to-time day)
			     for decoded = (decode-time time)
			     when (funcall (nth 2 (car limits)) decoded)
			     collect day))
	   (count (length x-ticks))
	   (print-format (nth 1 (car limits)))
	   (max-print (eplot--format-value (car x-ticks) print-format
					   x-label-format))
	   (min-spacing (* 1.2 (eplot--text-width max-print label-font
						  label-font-size))))
      (cond
       ;; We have room for every X value.
       ((< (* count min-spacing) xs)
	(list x-ticks print-format))
       ;; We have to prune X labels, but not grid lines.  (We shouldn't
       ;; have a grid line more than every 10 pixels.)
       ((< (* count 10) xs)
	(cond
	 ((not (cdr limits))
	  (eplot--year-ticks
	   x-ticks xs label-font label-font-size x-label-format))
	 ;; The Mondays grid is special, because it doesn't resolve
	 ;; into any of the bigger limits evenly.
	 ((= (caar limits) (/ 368 4))
	  (let* ((max-print (eplot--format-value
			     (car x-ticks) print-format x-label-format))
		 (min-spacing (* 1.2 (eplot--text-width
				      max-print label-font label-font-size)))
		 (weed-factor 2))
	    (while (> (* (/ (length x-ticks) weed-factor) min-spacing) xs)
	      (setq weed-factor (* weed-factor 2)))
	    (list x-ticks 'date
		  (cl-loop for val in x-ticks
			   for i from 0
			   collect (list val t (zerop (% i weed-factor)))))))
	 (t
	  (pop limits)
	  (catch 'found
	    (while limits
	      (let ((candidate
		     (cl-loop for day in x-ticks
			      for time = (eplot--days-to-time day)
			      for decoded = (decode-time time)
			      collect (list day t
					    (not (not
						  (funcall (nth 2 (car limits))
							   decoded)))))))
		(setq print-format (nth 1 (car limits)))
		(let* ((max-print (eplot--format-value
				   (car x-ticks) print-format x-label-format))
		       (min-spacing (* 1.2 (eplot--text-width
					    max-print label-font
					    label-font-size)))
		       (num-labels (seq-count (lambda (v) (nth 2 v))
					      candidate)))
		  (when (and (not (zerop num-labels))
			     (< (* num-labels min-spacing) xs))
		    (throw 'found (list x-ticks print-format candidate)))))
	      (pop limits))
	    (eplot--year-ticks
	     x-ticks xs label-font label-font-size x-label-format)))))
       ;; We have to reduce both grid lines and labels.
       (t
	(eplot--get-date-ticks start end xs label-font label-font-size
			       x-label-format (caar limits)))))))

(defun eplot--year-ticks (x-ticks xs label-font label-font-size x-label-format)
  (let* ((year-ticks (mapcar (lambda (day)
			       (decoded-time-year
				(decode-time (eplot--days-to-time day))))
			     x-ticks))
	 (xv (eplot--compute-x-ticks
	      xs year-ticks 'year x-label-format label-font label-font-size)))
    (let ((tick-step (car xv))
	  (label-step (cadr xv)))
      (list x-ticks 'year
	    (cl-loop for year in year-ticks
		     for val in x-ticks
		     collect (list val
				   (zerop (% year tick-step))
				   (zerop (% year label-step))))))))

(defun eplot--get-time-ticks (start end xs label-font label-font-size
				    x-label-format
				    &optional skip-until)
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
	   (max-print (eplot--format-value (car x-ticks) print-format
					   x-label-format))
	   (min-spacing (* (+ (length max-print) 2) (e/ label-font-size 2))))
      (cond
       ;; We have room for every X value.
       ((< (* count min-spacing) xs)
	(list x-ticks print-format))
       ;; We have to prune X labels, but not grid lines.  (We shouldn't
       ;; have a grid line more than every 10 pixels.)
       ;; If we're plotting just seconds, then just weed out some seconds.
       ((and (< (* count 10) xs)
	     (= (caar limits) (* 2 60)))
	(let ((xv (eplot--compute-x-ticks
		   xs x-ticks 'time x-label-format label-font label-font-size)))
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
	    (eplot--hour-ticks x-ticks xs label-font label-font-size
			       x-label-format)
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
				      (e/ label-font-size 2))))
		  (when (< (* (seq-count (lambda (v) (nth 2 v)) candidate)
			      min-spacing)
			   xs)
		    (throw 'found (list x-ticks print-format candidate)))))
	      (pop limits))
	    (eplot--hour-ticks x-ticks xs label-font label-font-size
			       x-label-format))))
       ;; We have to reduce both grid lines and labels.
       (t
	(eplot--get-time-ticks start end xs label-font label-font-size
			       x-label-format (caar limits)))))))

(defun eplot--hour-ticks (x-ticks xs label-font label-font-size
				  x-label-format)
  (let* ((eplot--pleasing-numbers '(1 3 6 12))
	 (hour-ticks (mapcar (lambda (time)
			       (decoded-time-hour (decode-time time)))
			     x-ticks))
	 (xv (eplot--compute-x-ticks
	      xs hour-ticks 'year x-label-format label-font label-font-size)))
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
    (setq-local eplot--current-chart
		(eplot--render (with-temp-buffer
				 (insert-file-contents file)
				 (eplot--parse-buffer))))))

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
  (dolist (header (sort (copy-sequence headers)
			(lambda (e1 e2)
			  (string< (car e1) (car e2)))))
    (insert (propertize (capitalize (symbol-name (car header))) 'face 'bold)
	    "\n")
    (let ((start (point)))
      (insert (plist-get (cdr header) :doc) "\n")
      (when-let ((valid (plist-get (cdr header) :valid)))
	(insert "Possible values are: "
		(mapconcat (lambda (v) (format "`%s'" v)) valid ", ")
		".\n"))
      (indent-rigidly start (point) 2))
    (ensure-empty-lines 1)))

(defvar eplot--transients
  '((("Size"
      ("sw" "Width")
      ("sh" "Height")
      ("sl" "Margin-Left")
      ("st" "Margin-Top")
      ("sr" "Margin-Right")
      ("sb" "Margin-Bottom"))
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
      ("bi" "Background-Image-File")
      ("bv" "Background-Image-Cover")
      ("bo" "Background-Image-Opacity")))
    (("General"
      ("gt" "Title")
      ("gf" "Font")
      ("gs" "Font-Size")
      ("ge" "Font-Weight")
      ("go" "Format")
      ("gw" "Frame-Width")
      ("gh" "Header-File")
      ("gi" "Min")
      ("ga" "Max")
      ("gm" "Mode")
      ("gr" "Reset" eplot--reset-transient)
      ("gv" "Save" eplot--save-transient))
     ("Axes, Grid & Legend"
      ("tf" "Title-Font")
      ("ts" "Title-Font-Size")
      ("xx" "X-Title")
      ("xy" "Y-Title")
      ("xf" "Label-Font")
      ("xz" "Label-Font-Size")
      ("xs" "X-Axis-Title-Space")
      ("xl" "X-Label-Format")
      ("xa" "Y-Label-Format")
      ("il" "Grid-Color")
      ("io" "Grid-Opacity")
      ("ip" "Grid-Position")
      ("ll" "Legend")
      ("lb" "Legend-Background-Color")
      ("lo" "Legend-Border-Color")
      ("lc" "Legend-Color"))
     ("Plot"
      ("ps" "Style")
      ("pc" "Color")
      ("po" "Data-Column")
      ("pr" "Data-format")
      ("pn" "Fill-Border-Color")
      ("pi" "Fill-Color")
      ("pg" "Gradient")
      ("pz" "Size")
      ("pm" "Smoothing")
      ("pb" "Bezier-Factor")))))

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
	;; Allow explicit commands.
	(or (nth 2 action)
	    ;; Make a command for altering a setting.
	    (lambda ()
	      (interactive)
	      (eplot--execute-transient (nth 1 action))))))

(defun eplot--execute-transient (action)
  (with-current-buffer (or eplot--data-buffer (current-buffer))
    (unless eplot--transient-settings
      (setq-local eplot--transient-settings nil))
    (let* ((name (intern (downcase action)))
	   (spec (assq name (append eplot--chart-headers eplot--plot-headers)))
	   (type (plist-get (cdr spec) :type)))
      ;; Sanity check.
      (unless spec
	(error "No such header type: %s" name))
      (setq eplot--transient-settings
	    (append
	     eplot--transient-settings
	     (list
	      (cons
	       name
	       (cond
		((eq type 'number)
		 (read-number (format "Value for %s (%s): " action type)))
		((string-match "color" (downcase action))
		 (eplot--read-color (format "Value for %s (color): " action)))
		((string-match "font" (downcase action))
		 (eplot--read-font-family
		  (format "Value for %s (font family): " action)))
		((string-match "gradient" (downcase action))
		 (eplot--read-gradient action))
		((string-match "file" (downcase action))
		 (read-file-name (format "File for %s: " action)))
		((eq type 'symbol)
		 (intern
		  (completing-read (format "Value for %s: " action)
				   (plist-get (cdr spec) :valid)
				   nil t)))
		(t
		 (read-string (format "Value for %s (string): " action))))))))
      (eplot-update-view-buffer))))

(defun eplot--read-gradient (action)
  (format "%s %s %s %s"
	  (eplot--read-color (format "%s from color: " action))
	  (eplot--read-color (format "%s to color: " action))
	  (completing-read (format "%s direction: " action)
			   '(top-down bottom-up left-right right-left)
			   nil t)
	  (completing-read (format "%s position: " action)
			   '(below above)
			   nil t)))

(defun eplot--reset-transient ()
  (interactive)
  (with-current-buffer (or eplot--data-buffer (current-buffer))
    (setq-local eplot--transient-settings nil)
    (eplot-update-view-buffer)))

(defun eplot--save-transient (file)
  (interactive "FSave parameters to file: ")
  (when (and (file-exists-p file)
	     (not (yes-or-no-p "File exists; overwrite? ")))
    (user-error "Exiting"))
  (let ((settings (with-current-buffer (or eplot--data-buffer (current-buffer))
		    eplot--transient-settings)))
    (with-temp-buffer
      (cl-loop for (name . value) in settings
	       do (insert (capitalize (symbol-name name)) ": "
			  (format "%s" value) "\n"))
      (write-region (point-min) (point-max) file))))

(defvar-keymap eplot-control-mode-map
  "RET" #'eplot-control-update
  "TAB" #'eplot-control-next-field
  "C-<tab>" #'eplot-control-next-field
  "<backtab>" #'eplot-control-prev-field)

(define-derived-mode eplot-control-mode special-mode "eplot control"
  (setq-local completion-at-point-functions
	      (cons 'eplot--complete-control completion-at-point-functions))
  (add-hook 'before-change-functions #'eplot--process-text-input-before nil t)
  (add-hook 'after-change-functions #'eplot--process-text-value nil t)
  (add-hook 'after-change-functions #'eplot--process-text-input nil t)
  (setq-local nobreak-char-display nil)
  (setq truncate-lines t))

(defun eplot--complete-control ()
  ;; Complete headers names.
  (when-let* ((input (get-text-property (point) 'input))
	      (name (plist-get input :name))
	      (spec (cdr (assq name (append eplot--plot-headers
					    eplot--chart-headers))))
	      (start (plist-get input :start))
	      (end (- (plist-get input :end) 2))
	      (completion-ignore-case t))
    (skip-chars-backward " " start)
    (or
     (and (eq (plist-get spec :type) 'symbol)
	  (lambda ()
	    (let ((valid (plist-get spec :valid)))
	      (completion-in-region
	       (save-excursion
		 (skip-chars-backward "^ " start)
		 (point))
	       end
	       (mapcar #'symbol-name valid))
	      'completion-attempted)))
     (and (string-match "color" (symbol-name name))
	  (lambda ()
	    (completion-in-region
	     (save-excursion
	       (skip-chars-backward "^ " start)
	       (point))
	     end eplot--colors)
	    'completion-attempted))
     (and (string-match "\\bfile\\b" (symbol-name name))
	  (lambda ()
	    (completion-in-region
	     (save-excursion
	       (skip-chars-backward "^ " start)
	       (point))
	     end (directory-files "."))
	    'completion-attempted))
     (and (string-match "\\bfont\\b" (symbol-name name))
	  (lambda ()
	    (completion-in-region
	     (save-excursion
	       (skip-chars-backward "^ " start)
	       (point))
	     end
	     (eplot--font-families))
	    'completion-attempted)))))

(defun eplot--read-font-family (prompt)
  "Prompt for a font family, possibly offering autocomplete."
  (let ((families (eplot--font-families)))
    (if families
	(completing-read prompt families)
      (read-string prompt))))

(defun eplot--font-families ()
  (when (executable-find "fc-list")
    (let ((fonts nil))
      (with-temp-buffer
	(call-process "fc-list" nil t nil ":" "family")
	(goto-char (point-min))
	(while (re-search-forward "^\\([^,\n]+\\)" nil t)
	  (push (downcase (match-string 1)) fonts)))
      (seq-uniq (sort fonts #'string<)))))

(defun eplot-control-next-input ()
  "Go to the next input field."
  (interactive)
  (when-let ((match (text-property-search-forward 'input)))
    (goto-char (prop-match-beginning match))))

(defun eplot-control-update ()
  "Update the chart based on the current settings."
  (interactive)
  (let ((settings nil))
    (save-excursion
      (goto-char (point-min))
      (while-let ((match (text-property-search-forward 'input)))
	(when (equal (get-text-property (prop-match-beginning match) 'face)
		     'eplot--input-changed)
	  (let* ((name (plist-get (prop-match-value match) :name))
		 (spec (cdr (assq name (append eplot--plot-headers
					       eplot--chart-headers))))
		 (value
		  (or (plist-get (prop-match-value match) :value)
		      (plist-get (prop-match-value match) :original-value))))
	    (setq value (string-trim (string-replace "\u00A0" " " value)))
	    (push (cons name
			(cl-case (plist-get spec :type)
			  (number
			   (string-to-number value))
			  (symbol
			   (intern (downcase value)))
			  (symbol-list
			   (mapcar #'intern (split-string (downcase value))))
			  (t
			   value)))
		  settings)))))
    (with-current-buffer eplot--data-buffer
      (setq-local eplot--transient-settings (nreverse settings))
      (eplot-update-view-buffer))))

(defvar eplot--column-width nil)

(defun eplot-create-controls ()
  "Pop to a buffer that lists all parameters and allows editing."
  (interactive)
  (with-current-buffer (or eplot--data-buffer (current-buffer))
    (let ((settings eplot--transient-settings)
	  (data-buffer (current-buffer))
	  (chart eplot--current-chart)
	  ;; Find the max width of all the different names.
	  (width (seq-max
		  (mapcar (lambda (e)
			    (length (cadr e)))
			  (apply #'append
				 (mapcar #'cdr
					 (apply #'append eplot--transients))))))
	  (transients (mapcar #'copy-sequence
			      (copy-sequence eplot--transients))))
      (unless chart
	(user-error "Must be called from an eplot buffer that has rendered a chart"))
      ;; Rearrange the transients a bit for better display.
      (let ((size (caar transients)))
	(setcar (car transients) (caadr transients))
	(setcar (cadr transients) size))
      (pop-to-buffer "*eplot controls*")
      (unless (eq major-mode 'eplot-control-mode)
	(eplot-control-mode))
      (setq-local eplot--data-buffer data-buffer
		  eplot--column-width (+ width 12 2))
      (let ((inhibit-read-only t)
	    (before-change-functions nil)
	    (after-change-functions nil))
	(erase-buffer)
	(cl-loop for column in transients
		 for cn from 0
		 do
		 (goto-char (point-min))
		 (end-of-line)
		 (cl-loop
		  for row in column
		  do
		  (if (zerop cn)
		      (when (not (bobp))
			(insert (format (format "%%-%ds" (+ width 14)) "")
				"\n"))
		    (unless (= (count-lines (point-min) (point)) 1)
		      (if (eobp)
			  (progn
			    (insert (format (format "%%-%ds" (+ width 14)) "")
				    "\n")
			    (insert (format (format "%%-%ds" (+ width 14)) "")
				    "\n")
			    (forward-line -1)
			    (end-of-line))
			(forward-line 1)
			(end-of-line))))
		  ;; If we have a too-long input in the first column,
		  ;; then go to the next line.
		  (when (and (> cn 0)
			     (> (- (point) (pos-bol))
				(+ width 12 2)))
		    (forward-line 1)
		    (end-of-line))
		  (insert (format (format "%%-%ds" (+ width 14))
				  (propertize (pop row) 'face 'bold)))
		  (if (looking-at "\n")
		      (forward-line 1)				
		    (insert "\n"))
		  (cl-loop
		   for elem in row
		   for name = (cadr elem)
		   for slot = (intern (downcase name))
		   when (null (nth 2 elem))
		   do
		   (let* ((object (if (assq slot eplot--chart-headers)
				      chart
				    (car (slot-value chart 'plots))))
			  (value (eplot--format-control-value
				  (or (cdr (assq slot settings))
				      (if (not (slot-boundp object slot))
					  ""
					(or (slot-value object slot)
					    ""))))))
		     (end-of-line)
		     ;; If we have a too-long input in the first column,
		     ;; then go to the next line.
		     (when (and (> cn 0)
				(> (- (point) (pos-bol))
				   (+ width 12 2)))
		       (forward-line 1)
		       (end-of-line))
		     (when (and (> cn 0)
				(bolp))
		       (insert (format (format "%%-%ds" (+ width 14)) "") "\n")
		       (forward-line -1)
		       (end-of-line))
		     (insert (format (format "%%-%ds" (1+ width)) name))
		     (eplot--input slot value
				   (if (cdr (assq slot settings))
				       'eplot--input-changed
				     'eplot--input-default))
		     (if (looking-at "\n")
			 (forward-line 1)				
		       (insert "\n")))))))
      (goto-char (point-min)))))

(defun eplot--format-control-value (val)
  (if (floatp val)
      ;; We want to avoid "59.9999999" and the like, so we just format
      ;; a very long floating point number and then strip the trailing
      ;; zeros.  Stupid, but it works.
      (replace-regexp-in-string "0+$" "" (format "%.10f" val))
    (format "%s" val)))

(defface eplot--input-default
  '((t :background "#505050"
       :foreground "#a0a0a0"
       :box (:line-width 1)))
  "Face for eplot default inputs.")

(defface eplot--input-changed
  '((t :background "#505050"
       :foreground "white"
       :box (:line-width 1)))
  "Face for eplot changed inputs.")

(defvar-keymap eplot--input-map
  :full t :parent text-mode-map
  "RET" #'eplot-control-update
  "TAB" #'eplot-input-complete
  "C-a" #'eplot-move-beginning-of-input
  "C-e" #'eplot-move-end-of-input
  "C-k" #'eplot-kill-input
  "C-<tab>" #'eplot-control-next-field
  "<backtab>" #'eplot-control-prev-field)

(defun eplot-input-complete ()
  "Complete values in inputs."
  (interactive)
  (cond
   ((let ((completion-fail-discreetly t))
      (completion-at-point))
    ;; Completion was performed; nothing else to do.
    nil)
   ((not (get-text-property (point) 'input))
    (eplot-control-next-input))
   (t
    (user-error "No completion in this field"))))

(defun eplot-move-beginning-of-input ()
  "Move to the start of the current input field."
  (interactive)
  (if (= (point) (eplot--beginning-of-field))
      (goto-char (pos-bol))
    (goto-char (eplot--beginning-of-field))))
  
(defun eplot-move-end-of-input ()
  "Move to the end of the current input field."
  (interactive)
  (let ((input (get-text-property (point) 'input)))
    (if (or (not input)
	    (= (point) (1- (plist-get input :end))))
	(goto-char (pos-eol))
      (goto-char (1+ (eplot--end-of-field))))))

(defun eplot-control-next-field ()
  "Move to the beginning of the next field."
  (interactive)
  (let ((input (get-text-property (point) 'input))
	(start (point)))
    (when input
      (goto-char (plist-get input :end)))
    (let ((match (text-property-search-forward 'input)))
      (if match
	  (goto-char (prop-match-beginning match))
	(goto-char start)
	(user-error "No next field")))))

(defun eplot-control-prev-field ()
  "Move to the beginning of the previous field."
  (interactive)
  (let ((input (get-text-property (point) 'input))
	(start (point)))
    (when input
      (goto-char (plist-get input :start))
      (unless (bobp)
	(forward-char -1)))
    (let ((match (text-property-search-backward 'input)))
      (unless match
	(goto-char start)
	(user-error "No previous field")))))

(defun eplot-kill-input ()
  "Remove the part of the input after point."
  (interactive)
  (let ((end (1+ (eplot--end-of-field))))
    (kill-new (string-trim (buffer-substring (point) end)))
    (delete-region (point) end)))

(defun eplot--input (name value face)
  (let ((start (point))
	input)
    (insert value)
    (when (< (length value) 11)
      (insert (make-string (- 11 (length value)) ?\u00A0)))
    (put-text-property start (point) 'face face)
    (put-text-property start (point) 'inhibit-read-only t)
    (put-text-property start (point) 'input
		       (setq input
			     (list :name name
				   :size 11
				   :is-default (eq face 'eplot--input-default)
				   :original-value value
				   :original-face face
				   :start (set-marker (make-marker) start)
				   :value value)))
    (put-text-property start (point) 'local-map eplot--input-map)
    ;; This seems like a NOOP, but redoing the properties like this
    ;; somehow makes `delete-region' work better.
    (set-text-properties start (point) (text-properties-at start))
    (insert (propertize " " 'face face
			'input input
			'inhibit-read-only t
			'local-map eplot--input-map))
    (plist-put input :end (point-marker))
    (insert " ")))

(defun eplot--end-of-field ()
  (- (plist-get (get-text-property (point) 'input) :end) 2))

(defun eplot--beginning-of-field ()
  (plist-get (get-text-property (point) 'input) :start))

(defvar eplot--prev-deletion nil)

(defun eplot--process-text-input-before (beg end)
  (message "Before: %s %s" beg end)
  (cond
   ((= beg end)
    (setq eplot--prev-deletion nil))
   ((> end beg)
    (setq eplot--prev-deletion (buffer-substring beg end)))))

(defun eplot--process-text-input (beg end _replace-length)
  ;;(message "After: %s %s %s %s" beg end replace-length eplot--prev-deletion)
  (when-let ((props (if eplot--prev-deletion
 			(text-properties-at 0 eplot--prev-deletion)
 		      (if (get-text-property end 'input)
 			  (text-properties-at end)
 			(text-properties-at beg))))
 	     (input (plist-get props 'input)))
    ;; The action concerns something in the input field.
    (let ((buffer-undo-list t)
	  (inhibit-read-only t)
	  (size (plist-get input :size)))
      (save-excursion
	(set-text-properties beg (- (plist-get input :end) 2) props)
	(goto-char (1- (plist-get input :end)))
	(let* ((remains (- (point) (plist-get input :start) 1))
	       (trim (- size remains 1)))
	  (if (< remains size)
	      ;; We need to add some padding.
	      (insert (apply #'propertize (make-string trim ?\u00A0)
			     props))
	    ;; We need to delete some padding, but only delete
	    ;; spaces at the end.
	    (setq trim (abs trim))
	    (while (and (> trim 0)
			(eql (char-after (1- (point))) ?\u00A0))
	      (delete-region (1- (point)) (point))
	      (cl-decf trim))
	    (when (> trim 0)
	      (eplot--possibly-open-column)))))
      ;; We re-set the properties so that they are continguous.  This
      ;; somehow makes the machinery that decides whether we can kill
      ;; a word work better.
      (set-text-properties (plist-get input :start)
			   (1- (plist-get input :end)) props)
      ;; Compute what the value is now.
      (let ((value (buffer-substring-no-properties
		    (plist-get input :start)
		    (plist-get input :end))))
	(when (string-match "\u00A0+\\'" value)
	  (setq value (substring value 0 (match-beginning 0))))
	(plist-put input :value value)))))

(defun eplot--possibly-open-column ()
  (save-excursion
    (when-let ((input (get-text-property (point) 'input)))
      (goto-char (plist-get input :end)))
    (unless (looking-at " *\n")
      (skip-chars-forward " ")
      (while (not (eobp))
	(let ((text (buffer-substring (point) (pos-eol))))
	  (delete-region (point) (pos-eol))
	  (forward-line 1)
	  (if (eobp)
	      (insert (make-string eplot--column-width ?\s) text "\n")
	    (forward-char eplot--column-width)
	    (if (get-text-property (point) 'input)
		(forward-line 1)
	      (insert text)
	      ;; We have to fix up the markers.
	      (save-excursion
		(let* ((match (text-property-search-backward 'input))
		       (input (prop-match-value match)))
		  (plist-put input :start
			     (set-marker (plist-get input :start)
					 (prop-match-beginning match)))
		  (plist-put input :end
			     (set-marker (plist-get input :end)
					 (+ (prop-match-end match) 1))))))))))))

(defun eplot--process-text-value (beg _end _replace-length)
  (when-let* ((input (get-text-property beg 'input)))
    (let ((inhibit-read-only t))
      (when (plist-get input :is-default)
	(put-text-property (plist-get input :start)
			   (plist-get input :end)
			   'face
			   (if (equal (plist-get input :original-value)
				      (plist-get input :value))
			       'eplot--input-default
			     'eplot--input-changed))))))

(defun eplot--read-color (prompt)
  "Read an SVG color."
  (completing-read prompt eplot--colors))

(eval `(transient-define-prefix eplot-customize ()
	 "Customize Chart"
	 ,@(eplot--define-transients)))

(defun eplot--bezier (factor i points)
  (cl-labels ((padd (p1 p2)
		(cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))
	      (psub (p1 p2)
		(cons (- (car p1) (car p2)) (- (cdr p1) (cdr p2))))
	      (pscale (factor point)
		(cons (* factor (car point)) (* factor (cdr point)))))
    (let* ((start (elt points (1- i)))
	   (end (elt points i))
	   (prev (if (< (- i 2) 0)
		     start
		   (elt points (- i 2))))
	   (next (if (> (1+ i) (1- (length points)))
		     end
		   (elt points (1+ i))))
	   (start-control-point
	    (padd start (pscale factor (psub end prev))))
	   (end-control-point
	    (padd end (pscale factor (psub start next)))))
      (list (car start-control-point)
	    (cdr start-control-point)
	    (car end-control-point)
	    (cdr end-control-point)
	    (car end)
	    (cdr end)))))

;;; CSV Parsing Stuff.

(defun eplot--csv-buffer-p ()
  (save-excursion
    (goto-char (point-min))
    (let ((min 1.0e+INF)
	  (max -1.0e+INF)
	  (total 0)
	  (lines 0))
      (while (not (eobp))
	(let ((this 0))
	  (while (search-forward "," (pos-eol) t)
	    (cl-incf total)
	    (cl-incf this))
	  (forward-line 1)
	  (cl-incf lines)
	  (setq min (min min this)
		max (max max this))))
      (let ((mid (e/ total lines)))
	;; If we have a comma on each line, and it's fairly evenly
	;; distributed, it's a CSV buffer.
	(and (>= min 1)
	     (< (* mid 0.9) min)
	     (> (* mid 1.1) max))))))

(defun eplot--numericalp (value)
  (string-match-p "\\`[-.0-9]*\\'" value))

(defun eplot--numberish (value)
  (if (or (zerop (length value))
	  (not (eplot--numericalp value)))
      value
    (string-to-number value)))

(defun eplot--parse-csv-buffer ()
  (unless (fboundp 'pcsv-parse-buffer)
    (user-error "You need to install the pcsv package to parse CSV files"))
  (let ((csv (and (fboundp 'pcsv-parse-buffer)
		  ;; This repeated check is just to silence the byte
		  ;; compiler.
		  (pcsv-parse-buffer)))
	names)
    ;; Check whether we have a common string/number CSV file.
    ;; The second number in every line is numerical...
    (if (and (cl-every #'eplot--numericalp (mapcar #'cadr (cdr csv)))
	     ;; But some alphabetical first columns.
	     (not (cl-every #'eplot--numericalp (mapcar #'car (cdr csv)))))
	(progn
	  ;; Check whether first line is a header.
	  (when (not (eplot--numericalp (cadr (car csv))))
	    (setq names (pop csv)))
	  (list
	   (cons 'format "bar-chart")
	   (cons 'y-title (nth 1 names))
	   (cons 'x-title (nth 0 names))
	   (cons :plots
		 (cl-loop
		  for column from 1 upto (1- (length (car csv)))
		  collect
		  (list (cons
			 :headers
			 (list
			  (cons 'name (elt names column))
			  (cons 'data-format "number")
			  (cons 'color "vary")))
			(cons
			 :values
			 (cl-loop for line in csv
				  collect (list
					   :settings
					   `((label . ,(car line)))
					   :value (eplot--numberish
						   (elt line column))))))))))
      ;; Other CSV format.
      ;; Check whether the first line looks like a header.
      (when (and (length> csv 1)
		 ;; The second line is all numbers...
		 (cl-every #'eplot--numericalp (nth 1 csv))
		 ;; .. and the first line isn't.
		 (not (cl-every #'eplot--numericalp (nth 0 csv))))
	(setq names (pop csv)))
      (list
       (cons 'legend (if names "true" "false"))
       (cons :plots
	     (cl-loop
	      for column from 1 upto (1- (length (car csv)))
	      collect
	      (list (cons :headers
			  (list
			   (cons 'name (elt names column))
			   (cons 'data-format
				 (cond
				  ((cl-every (lambda (e) (<= (length e) 4))
					     (mapcar #'car csv))
				   "year")
				  ((cl-every (lambda (e) (= (length e) 8))
					     (mapcar #'car csv))
				   "date")
				  (t
				   "number")))
			   (cons 'color (eplot--vary-color "vary" (1- column)))))
		    (cons
		     :values
		     (cl-loop for line in csv
			      collect (list :x (eplot--numberish (car line))
					    :value (eplot--numberish
						    (elt line column))))))))))))

(declare-function org-element-parse-buffer "org-element")

(defun eplot--parse-org-buffer ()
  (require 'org-element)
  (let* ((table (nth 2 (nth 2 (org-element-parse-buffer))))
	 (columns (cl-loop for cell in (nthcdr 2 (nth 2 table))
			   collect (substring-no-properties (nth 2 cell))))
	 (value-column (or (seq-position columns "value") 0))
	 (date-column (seq-position columns "date")))
    `((:plots
       ((:headers
	 ,@(and date-column '((data-format . "date"))))
	(:values 
	 ,@(cl-loop for row in (nthcdr 4 table)
		    collect
		    (let ((cells (cl-loop for cell in (nthcdr 2 row)
					  collect (substring-no-properties
						   (nth 2 cell)))))
		      (list :value (string-to-number (elt cells value-column))
			    :x (string-to-number
				(replace-regexp-in-string
				 "[^0-9]" "" (elt cells date-column)))
			    )))))))))

(provide 'eplot)

;;; eplot.el ends here
