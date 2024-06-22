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

