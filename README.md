eplot is an Emacs package for generating time series charts, plots and
bar charts interactively.

This is a work in progress and is probably not very useful generally
yet, but here's the documentation so far:

![movie-plot.plt](/images/movie-plot.svg)

Here's an example...

![chart-legend.plt](/images/chart-legend.svg)

And here's another.

But let's start from the start.  Here's a trivial example,
trivial-very-short.plt:

	41
	42.1
	44.7

Then say `M-x eplot' in this buffer, and you'll get this displayed:

![trivial-very-short.plt](/images/trivial-very-short.svg)

If you have the pcsv package installed, you can also do CSV files.

	Year,explore,investigate
	2000,1834.6246923361425,3314.978325069512
	2001,1973.1940690964868,3484.9536191658754
	2002,2128.9463492787686,3671.1459598826505
	...

Just say `M-x eplot' in a buffer with values like that, and eplot will
do its best to display the data.

To tweak the look of the chart, there are three possibilities:

* You can use the `c' command in the eplot buffer or the `C-c C-v'
  command in the plt buffer.  This command prompts you for values
  using a Transient interface.
  
* You can use the `l' command in the eplot buffer.  This pops up an
  auxiliary "Controls" buffer that lists all the default different
  things that can be changed.  Hit the Return key after changing
  anything to see an updated chart.
  
* Finally, you can put headers into the file that describes the look
  of the chart.

Below, this final method is used in all the examples, because that's
convenient when going through lots of different settings in the
manual, but I'm guessing most users might want to use one of the two
first methods (which can be mixed -- you can do some settings via
Transient and some via the Controls buffer).

In most of the examples below, the following header lines are present,
and are not included in the code examples:

	Layout: compact
	Width: 300
	Height: 200

By default, eplot uses a more spacious layout, and the width/heights
are automatically calculated based on the window size.  But for these
examples in this manual, it's more convenient to show smaller, more
compact charts.

You don't need to have the actual chart data in the .plt file --
instead you can specify the location with a Data-File: plot header:

	Data-File: data1.txt

![chart-simple.plt](/images/chart-simple.svg)

This is the basic, default look of that data set.

OK, let's go through all the various options you have to change the
look of the charts.

First of all, several different plotting styles are available.  The
default is the "line" Style.

	Style: square

![chart-square.plt](/images/chart-square.svg)

	Style: curve

![chart-curve1.plt](/images/chart-curve1.svg)

This uses a Bezier curve to draw a smooth line between points.  It
takes a Bezier-Factor.  If you up this too much, you get amusing
results:

	Style: curve
	Gradient: red black
	Bezier-Factor: 0.8

![chart-curve2.plt](/images/chart-curve2.svg)

And you can add a gradient to curve plots, too.

	Style: triangle
	Fill-Color: #c0c0c0

![chart-triangle.plt](/images/chart-triangle.svg)

Some styles also take a Fill parameter to specify the fill color.

	Style: rectangle

![chart-rectangle.plt](/images/chart-rectangle.svg)

	Size: 3
	Color: red black green blue brown
	Style: impulse

![chart-impulse-wide.plt](/images/chart-impulse-wide.svg)

	Style: point

![chart-point.plt](/images/chart-point.svg)

	Style: cross

![chart-cross.plt](/images/chart-cross.svg)

	Style: circle
	Size: 8

![chart-circle.plt](/images/chart-circle.svg)

That's all the basic plotting styles.

	Data-File: data1.txt
	
	Style: impulse
	Color: blue
	Data-File: data2.txt

![chart-two-plots.plt](/images/chart-two-plots.svg)

You can have several plots in one chart, with their own styles and
colors.

	Y-Label-Format: %s%%
	X-Label-Format: $%s
	Data-File: data1.txt

![chart-format.plt](/images/chart-format.svg)

You can specify a format string for the X and Y labels.

	X-Label-Format: On %b %e

	Data-Format: date
	20090101 41
	20090102 42.1
	20090103 42.1

![chart-date1.plt](/images/chart-date1.svg)

If the chart
is a date/time format chart, the format string will be passed to
`format-time-string'.

	Format: bar-chart
	Color: vary
	Title: How Much Higher Would A Director Place
	Mode: dark
	X-Label-Orientation: vertical
	Margin-Bottom: 150

	1347 # Label: James Benning
	789 # Label: Raúl Ruiz
	782 # Label: Jim Jarmusch
	...
	
![directors2.plt](/images/directors2.svg)

In bar charts, you can place the X labels vertically.

	Format: horizontal-bar-chart
	Color: vary
	Mode: dark

	1347 # Label: James Benning
	789 # Label: Raúl Ruiz
	782 # Label: Jim Jarmusch
	...
	
![directors1.plt](/images/directors1.svg)

And you can have horizontal bar charts.

There's a special color spec called "vary", and it results in choosing
between some pre-defined legible colors:

	Style: circle
	Fill-Color: vary
	Color: none
	Size: 8

![chart-circle-colors.plt](/images/chart-circle-colors.svg)

The "vary" isn't useful on all plot styles:

	Color: vary
	Data-File: data1.txt

![chart-segment.plt](/images/chart-segment.svg)

You can also specify colors/sizes on a per-data point basis.  Here's
an excerpt from a plt file that demonstrates that:

	Style: circle
	Fill-Color: none
	41
	42.1
	42.1
	44.1
	43.6 # Size: 10, Fill-Color: red
	43.6
	42.9
	43
	43.2
	44.4
	44.5
	45.4
	45.9
	52   # Size: 10, Fill-Color: blue
	53

![chart-circle-sizes.plt](/images/chart-circle-sizes.svg)

It's sometimes useful to be able to single out certain values with a
text or a special mark.

	Style: curve
	Color: black
	41
	42.1
	42.1
	...
	53   # Text: New Model, Mark: cross, Size: 3
	52.5
	51
	...
	
![chart-texts.plt](/images/chart-texts.svg)

And as you can see, you can combine the text and the mark.

	Data-Format: date

	Style: circle
	Fill-Color: vary
	Size-Factor: 3
	...
	20240624 5
	20240728 6
	20240712 14
	
![chart-circle-size.plt](/images/chart-circle-size.svg)

You can also use a Size-Factor -- this means that each element will be
scaled along with their values.  This only makes sense for styles like
circle, rectangle and the like.

Bar charts are somewhat special:

	Format: bar-chart

	Color: vary
	33
	20
	24

![chart-bar.plt](/images/chart-bar.svg)

Each bar is displayed separately, and the labels on the X axis are
displayed in the middle of the bars -- this is quite different from
the box plot on the same data set:

	Format: normal
	Min: 0
	
	Style: square
	33
	20
	24

![chart-bar-box.plt](/images/chart-bar-box.svg)

Anyway, the first bar box demonstrated the "vary" color again, but you can
specify the colors yourself:

	Format: bar-chart

	Color: green gray
	33 # Label: Apples
	20 # Label: Oranges
	24 # Label: Appanges

![chart-color-list.plt](/images/chart-color-list.svg)

When it runs out of colors, it starts anew.  And another feature
demonstrated here is that you can specify the labels on a per-item
basis.

You can also have plots with two values per item, and eplot then fills
the data in between:

	Data-Format: two-values
	Style: square
	Fill-Color: lightblue
	Fill-Border-Color: black
	Data-File: data-between.txt

![chart-between2.plt](/images/chart-between2.svg)

	Mode: dark
	Border-Width: 1

	Data-Format: two-values
	Style: square
	Gradient: red black
	Data-File: data-between.txt

![chart-between1.plt](/images/chart-between1.svg)

This is the same data set, but in "dark mode", and with a gradient
between the values.  Oh, and a border, because why not.

	Data-Column: 2
	41     57.551391
	42.1    59.88006
	42.1    59.88006
	...

![chart-second-column.plt](/images/chart-second-column.svg)

It can sometimes be convenient to have several data sets in the same
file, but in different columns.  You can use the Data-Column header to
specify which column to plot.

OK, there's a lot of details about the plot you can specify.  Here's
an example:

	Border-Color: yellow
	Border-Width: 3
	Frame-Color: red
	Surround-Color: green
	Background-Color: blue
	Grid-Color: black
	Chart-Color: white
	Title: Title, background and border
	Axes-Color: black
	Title-Color: red

	Data-File: data1.txt

![chart-color.plt](/images/chart-color.svg)

Go wild!  But preferably not that wild.

	Gradient: black red
	Data-File: data1.txt

![chart-gradient1.plt](/images/chart-gradient1.png)

You can also plot the data using gradients.

	Gradient: black red
	Style: square
	Data-File: data1.txt

![chart-gradient-square.plt](/images/chart-gradient-square.png)

You can put the gradient above the chart instead:

	Gradient: black red top-down above
	Data-File: data1.txt

![chart-gradient2.plt](/images/chart-gradient2.png)

	Gradient: black red bottom-up
	Data-File: data1.txt

![chart-gradient3.plt](/images/chart-gradient3.png)

You can specify the direction -- this one is basically just swapping the
order of the colors.

	Gradient: black red right-left above
	Data-File: data1.txt

![chart-gradient4.plt](/images/chart-gradient4.png)

But also right-left...

	Gradient: black red left-right below
	Data-File: data1.txt

![chart-gradient6.plt](/images/chart-gradient6.png)

... and left-right.

	Gradient: black red bottom-up above
	Data-File: data1.txt
	
	Gradient: black red top-down below
	Data-File: data1.txt

![chart-gradient5.plt](/images/chart-gradient5.png)

And below the plot.

	Gradient: black red left-right above
	Data-File: data1.txt

![chart-two-gradient1.plt](/images/chart-two-gradient1.png)

You can plot the same data set twice with different gradients to get
interesting results.

	Gradient: black red left-right above
	Data-File: data1.txt
	
	Gradient: black red bottom-up
	Data-File: data1.txt

![chart-two-gradient2.plt](/images/chart-two-gradient2.png)

	Grid-Position: top
	Grid-Color: black
	
	Gradient: #000030 blue left-right above
	Data-File: data1.txt
	
	Gradient: black red top-down below
	Data-File: data1.txt

![chart-two-gradient3.plt](/images/chart-two-gradient3.png)

When using filled/gradient plots, it's often useful to put the grid on
top of the charts.

	Grid-Position: top
	Grid-Color: #803030
	
	Gradient: green blue top-down above
	Data-File: data1.txt
	
	Gradient: yellow red top-down below
	Data-File: data1.txt

![chart-two-gradient4.plt](/images/chart-two-gradient4.png)

Craazy.

	Grid-Position: top
	Grid-Color: black
	Grid-Opacity: 0.2

	Gradient: #000030 blue left-right above
	Data-File: data1.txt

	Gradient: black red top-down below
	Data-File: data1.txt

![chart-two-gradient5.plt](/images/chart-two-gradient5.png)

But when putting the grid on top like this, it's usually more visually
pleasant to make the grid non-opaque.

	Gradient: black 25-purple-50-white-75-purple-black
	Data-File: data1.txt

![chart-gradient-more-stops.plt](/images/chart-gradient-more-stops.png)

You can also have more stops.  In that case, the second element
specifies the percentage points of where each color ends, so the above
starts with black, then at 25% it's purple, then at 50% it's white,
then it's back to purple again at 75%, before ending up at black at a
100% (but you don't have to include the 100% here -- it's understood).

	X-Label: Day
	Y-Label: Money
	Margin-Left: 40
	Margin-Bottom: 40
	Grid-Position: top

	Data-File: data1.txt

![chart-labels.plt](/images/chart-labels.svg)

You can put labels on the axes.  If you're using the compact layout
(as we are in these examples), you have to open up some space for the
labels manually -- in the normal layout, there's already space for
labels.

	Data-Format: date
	Smoothing: moving-average
	20090101 157
	20090102 156
	20090103 152
	20090104 152
	...

![date-chart2.plt](/images/date-chart2.svg)

You can also use the date format to plot date series.  And another
feature demonstrated here is that you can have eplot smooth the data
before plotting.

	Data-Format: time
	144000 78.0
	145003 170.1
	150014 212.9
	...
	
![time-chart-long.plt](/images/time-chart-long.svg)

Similarly, you can also do intra-day time plots -- this takes a HHMMSS
format.

	Data-Format: xy
	Data-File: data-circle.txt

![chart-xy-circle.plt](/images/chart-xy-circle.svg)

eplot isn't really geared towards doing 2D plots -- it's for time
series and bar charts.  But it can do rudimentary 2D plots, but it
lacks some of the control you'd expect.

	Background-Image-File: background.jpg
	Grid-Color: #333
	Color: white
	Data-File: data1.txt
	
![chart-background1.plt](/images/chart-background1.svg)

You can put lovely wood grain images in the background to simulate
being back in the 70s.

	Background-Color: #888
	Background-Image-File: background2.jpg
	Background-Image-Cover: plot
	Background-Image-Opacity: 0.6
	Grid-Color: #333
	Color: white
	Data-File: data1.txt

![chart-background2.plt](/images/chart-background2.svg)

And you can put the background image over the entire chart, just
behind the plot, or just in the frame.  And also adjust the opacity.

	Chart-Color: white
	Grid-Color: #444
	Background-Gradient: blue black right-left
	Data-File: data1.txt
	Gradient: red black

![chart-gradient-background.plt](/images/chart-gradient-background.png)

You can also have a gradient over the entire background, of course.

	Header-File: external-headers.plt

If you're plotting a lot of charts, you usually want to have the same
basic parameters.  So you can put those in one file, and then include
them like this.  You can also use the `M-x eplot-with-headers'
command, which will plot using the data in the current buffer, but
taking the headers from that file.

So there's a lot of different headers you can use to influence the
look of the chart and the individual plots.  To get a list of them
all, and their possible values, use the `M-x eplot-list-chart-headers'
command.

You can have comments in a .plt buffer -- all lines that start with 
a # are comments.

eplot-mode
==========

The major mode for editing .plt files is very simple.  It does some
font-locking and then provides the following commands:

C-c C-c: Display a buffer with the rendered image.
C-c C-e: List all the different headers and document them.
TAB:     Autocomplete headers.

eplot-view-mode
===============

The buffer used to display the charts uses another simple major mode
with some convenient commands.

C-c C-s: Save the image to a file.
C-c C-w: Specify an image width and then save the file.

If saving to a different format than SVG, the "convert" or
"rsvg-convert" external programs are needed to do the conversion.

The C-c C-w command needs "rsvg-convert", because it renders the SVG
chart at a specific size.  This avoids scaling artefacts (blurring,
etc), and you get a crisp, nice chart at any size you require.

Installation
============

If you're installing this manually (I guess it'll be on an ELPA after
it's finished), put something like the following in your Emacs init file:

(push "~/src/eplot/" load-path)
(autoload 'eplot "eplot" nil t)
(autoload 'eplot-mode "eplot" nil t)
(unless (assoc "\\.plt" auto-mode-alist)
  (setq auto-mode-alist (cons '("\\.plt" . eplot-mode) auto-mode-alist)))

Adjust the path you're pushing to load-path depending on where you put
eplot.el.

API
===

(Probably more to come here...)

All the headers can have defaults set programmatically so that you can
define an overall look that you prefer (and then override with
explicit headers when you want).

This is done with the eplot-set function:

    (eplot-set 'width 600) 
    (eplot-set 'height 300)
    (eplot-set 'font "arial")
    (eplot-set 'background-color "#ffe0e0")
    (eplot-set 'grid-color "#c0c0c0")
  
![chart-simple.plt](/images/pink-plot.svg)

You can put this in your Emacs init file or somewhere, but then you
also need a

    (require 'eplot)
  
first.
