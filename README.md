eplot is an Emacs package for generating charts and plots
interactively.

Here's a trivial example, trivial.plt:

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

You can also specify colors/sizes on a per-data point basis.  Here's
an excerpt from a plt file that demonstrates that:

![chart-circle-colors.plt](/images/chart-circle-colors.png)

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

