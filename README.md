eplot is an Emacs package for generating charts and plots
interactively.

![movie-plot.plt](/images/movie-plot.png)

Here's an example...

![chart-legend.plt](/images/chart-legend.png)

And here's another.

But let's start from the start.  Here's a trivial example,
trivial.plt:

	41
	42.1
	44.7

Then say `M-x eplot' in this buffer, and you'll get this displayed:

![trivial.plt](/images/trivial.png)

To tweak the look of the chart, you add header lines to the buffer.
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
instead you can specify the location with a Data: plot header:

	Data: data1.txt

![chart-simple.plt](/images/chart-simple.png)

This is the basic, default look of that data set.

OK, let's go through all the various options you have to change the
look of the charts.

First of all, several different plotting styles are available.  The
default is the "line" Style.

	Style: square

![chart-square.plt](/images/chart-square.png)

	Style: triangle
	Fill: #c0c0c0

Some styles also take a Fill parameter to specify the fill color.

![chart-triangle.plt](/images/chart-triangle.png)

	Style: rectangle

![chart-rectangle.plt](/images/chart-rectangle.png)

	Style: impulse

![chart-impulse.plt](/images/chart-impulse.png)

	Style: point

![chart-point.plt](/images/chart-point.png)

	Style: cross

![chart-cross.plt](/images/chart-cross.png)

	Style: circle
	Size: 8

![chart-circle.plt](/images/chart-circle.png)

That's all the basic plotting styles.

You can have several plots in one chart, with their own styles and
colors.

	Data: data1.txt
	
	Style: impulse
	Color: blue
	Data: data2.txt

![chart-two-plots.plt](/images/chart-two-plots.png)

There's a special color spec called "vary", and it results in choosing
between some pre-defined legible colors:

	Style: circle
	Fill: vary
	Color: none
	Size: 8

![chart-circle-colors.plt](/images/chart-circle-colors.png)

The "vary" isn't useful on all plot styles:

	Color: vary
	Data: data1.txt

![chart-segment.plt](/images/chart-segment.png)

You can also specify colors/sizes on a per-data point basis.  Here's
an excerpt from a plt file that demonstrates that:

	Style: circle
	Fill: none
	41
	42.1
	42.1
	44.1
	43.6 # Size: 10, Fill: red
	43.6
	42.9
	43
	43.2
	44.4
	44.5
	45.4
	45.9
	52   # Size: 10, Fill: blue
	53

![chart-circle-sizes.plt](/images/chart-circle-sizes.png)

Bar charts are somewhat special.

	Format: bar-chart

	Color: vary
	33
	20
	24

![chart-bar.plt](/images/chart-bar.png)

Each bar is displayed separately, and the labels on the X axis are
displayed in the middle of the bars -- this is quite different from
the box plot:

	Format: normal
	Min: 0
	
	Style: square
	33
	20
	24

![chart-bar-box.plt](/images/chart-bar-box.png)

Anyway, the first bar box demonstrated the "vary" color, but you can
specify the colors yourself:

	Format: bar-chart

	Color: green gray
	33 # Label: Apples
	20 # Label: Oranges
	24 # Label: Appanges

![chart-color-list.plt](/images/chart-color-list.png)

When it runs out of colors, it starts anew.  And another feature
demonstrated here is that you can specify the labels on a per-item
basis.

You can also have plots with two values per item, and eplot then fills
the data in between:

	Data-Format: two-values
	Style: square
	Fill: lightblue
	Fill-Border: black
	Data: data-between.txt

![chart-between2.plt](/images/chart-between2.png)

	Mode: dark
	Border-Width: 1

	Data-Format: two-values
	Style: square
	Gradient: red black
	Data: data-between.txt

![chart-between1.plt](/images/chart-between1.png)

This is the same data set, but in "dark mode", and with a gradient
between the values.  Oh, and a border, because why not.

OK, there's a lot of details about the plot you can specify.  Here's
an example:

	Border-Color: yellow
	Border-Width: 3
	Frame-Color: red
	Surround-Color: green
	Background-Color: blue
	Grid-Color: black
	Color: white
	Title: Title, background and border
	Axes-Color: black
	Title-Color: red

	Data: data1.txt

![chart-color.plt](/images/chart-color.png)

Go wild!

You can also plot the data using gradients.

	Gradient: black red
	Style: square
	Data: data1.txt

![chart-gradient-square.plt](/images/chart-gradient-square.png)

	Gradient: black red
	Data: data1.txt

![chart-gradient1.plt](/images/chart-gradient1.png)

You can put the gradient above the chart instead:

	Gradient: black red top-down above
	Data: data1.txt

![chart-gradient2.plt](/images/chart-gradient2.png)

	Gradient: black red bottom-up
	Data: data1.txt

![chart-gradient3.plt](/images/chart-gradient3.png)

You can specify the direction -- this is basically just swapping the
order of the colors.

	Gradient: black red right-left above
	Data: data1.txt

![chart-gradient4.plt](/images/chart-gradient4.png)

But also right-left...

	Gradient: black red left-right below
	Data: data1.txt

![chart-gradient5.plt](/images/chart-gradient5.png)

... and left-right.

	Gradient: black red left-right above
	Data: data1.txt

![chart-gradient6.plt](/images/chart-gradient6.png)

And, of course, left-right above the chart.

	Gradient: black red bottom-up above
	Data: data1.txt
	
	Gradient: black red top-down below
	Data: data1.txt

![chart-two-gradient1.plt](/images/chart-two-gradient1.png)

You can plot the same data set twice with different gradients to get
interesting results.

	Gradient: black red left-right above
	Data: data1.txt
	
	Gradient: black red bottom-up
	Data: data1.txt

![chart-two-gradient2.plt](/images/chart-two-gradient2.png)

	Grid-Position: top
	Grid-Color: black
	
	Gradient: #000030 blue left-right above
	Data: data1.txt
	
	Gradient: black red top-down below
	Data: data1.txt

![chart-two-gradient3.plt](/images/chart-two-gradient3.png)

When using filled/gradient plots, it's often useful to put the grid on
top of the charts.

	Grid-Position: top
	Grid-Color: #803030
	
	Gradient: green blue top-down above
	Data: data1.txt
	
	Gradient: yellow red top-down below
	Data: data1.txt

![chart-two-gradient4.plt](/images/chart-two-gradient4.png)

Go wild.

	Grid-Position: top
	Grid-Color: black
	Grid-Opacity: 0.2

	Gradient: #000030 blue left-right above
	Data: data1.txt

	Gradient: black red top-down below
	Data: data1.txt

![chart-two-gradient5.plt](/images/chart-two-gradient5.png)

But when putting the grid on top like this, it's usually more visually
pleasant to make the grid non-opaque.

	X-Label: Day
	Y-Label: Money
	Margin-Left: 40
	Margin-Bottom: 40
	Grid-Position: top

	Data: data1.txt

![chart-labels.plt](/images/chart-labels.png)

You can put labels on the axes.  If you're using the compact layout
(as we are in these examples), you have to open up some space for the
labels manually -- in the normal layout, there's already space for
labels.
