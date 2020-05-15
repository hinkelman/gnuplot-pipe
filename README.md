# gnuplot-pipe

Port of [gnuplot-pipe](https://gitlab.com/montanari/gnuplot-pipe/) [egg](http://wiki.call-cc.org/eggref/5/gnuplot-pipe) for Chicken Scheme to Chez Scheme.

## Requirements

`gnuplot-pipe` requires [Gnuplot](http://gnuplot.info) (tested with version 5.2).

## Installation

Download or clone this repository. Move `gnuplot-pipe.sls` to a directory found by `(library-directories)`. For more information on how Chez Scheme finds libraries, see blog posts for [macOS and Windows](https://www.travishinkelman.com/post/getting-started-with-chez-scheme-and-emacs/) or [Ubuntu](https://www.travishinkelman.com/post/getting-started-with-chez-scheme-and-emacs-ubuntu/).

## Usage and examples

It is recommended to import `gnuplot-pipe` using a prefix, for example `gp:`.

```
(import (prefix (gnuplot-pipe) gp:))
```

All `gnuplot-pipe` procedures calls must be wrapped by

```
(gp:call/gnuplot
 ...)
 ```
 
The default Gnuplot executable name is `gnuplot`. A different executable name (signaled by a warning) may cause the interpreter to crash.

### Generic pipe to Gnuplot

The procedure `gp:send` is a simple pipe to gnuplot. Pass arbitrary command as string.

```
(gp:call/gnuplot
 (gp:send "set xlabel 'x'")
 (gp:send "plot sin(x)"))
```

### Plot 2D data and save to file

Plot list of numbers as x and y coordinates with `gp:plot`. A new Gnuplot window will open at each plot. 

```
(gp:call/gnuplot
 ;;; Plot x and y axes data as lines.
 (gp:plot "title 'x^2'" '(1 2 3 4) '(1 4 9 16)))
```

Save the last plot with `gp:save`.

```
(gp:call/gnuplot
 ;;; Plot multiple curves.
 (gp:plot '(("title 'x^2'" (1 2 3 4) (1 4 9 16))
            ("title 'x^3'" (1 2 3 4) (1 8 27 64))))

 ;;; Save last plot.
 (gp:save "plot.png"))
```

To avoid a persistent Gnuplot window after each plot (especially when running compiled code to save plots on file), set an interactive terminal (qt, wxt, x11, ...) as non-persistent. For example:

```
(gp:call/gnuplot
 (gp:send "set terminal x11 nopersist")
 ...)
```

### Plot 3D data

Plot 3D data with `gp:plot3d`.

```
(gp:call/gnuplot
 (gp:send "unset key")
 (gp:send "set style data points")
 (gp:send "set title 'The valley of the Gnu'")
 (gp:plot3d ""
            '(0 0 0 1 1 1 2 2 2 3 3 3)
            '(0 1 2 0 1 2 0 1 2 0 1 2)
            '(10 10 10 10 5 10 10 1 10 10 0 10)))
```

Draw multiple data similarly as for 2D plots.

### Understanding the plot procedures

Think of each list of numbers passed to `gp:plot` and `gp:plot3d` as a column passed as inline data in a Gnuplot script. The string passed to a plot element corresponds to properties optionally passed to the plot commands. For instance, the following Gnuplot script draws a pie chart with inline data.

```
set xrange [-15:15]
set style fill transparent solid 0.9 noborder
plot '-' using 1:2:3:4:5:6 with circles lc var
0    0    5    0    30    1
0    0    5   30    70    2
0    0    5   70   120    3
0    0    5  120   230    4
0    0    5  230   360    5
e
```

It is easily translated as follows.

```
(gp:call/gnuplot
 (gp:send "set xrange [-15:15]")
 (gp:send "set style fill transparent solid 0.9 noborder")
 (gp:plot "using 1:2:3:4:5:6 with circles lc var"
          '(0 0 0 0 0) '(0 0 0 0 0) '(5 5 5 5 5)
          '(0 30 70 120 230) '(30 70 120 230 360) '(1 2 3 4 5)))
```

For more involved plots, additional specifications may be required. Consider the following Gnuplot script with inline data where we need to specify the data end `e` twice:

```
plot ’-’ matrix with image
5 4 3 1 0
2 2 0 0 1
0 0 0 1 0
0 1 2 4 3
e
e
```

When drawing the same image with `gp:plot` we need to declare the second data end signal manually:

```
(gp:call/gnuplot
 (gp:plot "matrix with image"
          '(5 2 0 0) '(4 2 0 1) '(3 0 0 2) '(1 0 1 4) '(0 1 0 3))
 (gp:end-data)) ; Send end signal a second time manually.
```

## API

#### `gp:call/gnuplot`

```
[syntax] (call/gnuplot expr1 expr2 ...) 
```

Evaluate expressions redirecting output towards Gnuplot pipe. All procedures defined in this API must be used as `gp:call/gnuplot` expressions.

#### `gp:send`

```
[procedure] (send cmdline)
```

Send arbitrary command to Gnuplot as string.

#### `gp:plot`

```
[procedure] (plot . element)
```

Draw 2D data. This is a wrapper for the Gnuplot `plot` command offering many different graphical representations for data. A plot `element` can be given in the form:

```
(plot str list1 list2 ...)
```

where `str` is a (possibly empty) string with optional properties corresponding to Gnuplot axes `<axes>`, `<title-spec>` and with `<style>` specifications (see the Gnuplot manual or launch `gnuplot -e "help plot"` for more information). List of numbers `list1, list2, ...` are passed to Gnuplot as inline data columns.

To draw multiple sets of data, a plot element can be also given in the form:

```
(plot '((str-1 list1-1 list2-1 ...)
        (str-2 list1-2 list2-2 ...)
        ...))
```

#### `gp:plot3d`

```
[procedure] (plot3d . element) 
```

Draw 2D projections of 3D data. This is a wrapper for the Gnuplot `splot` command. Plot elements must be in the same format as in the `gp:plot` procedure.

#### `gp:save`

```
[procedure] (save fname)
```

Save last plot to file. Permitted file name `fname` extensions are: png, pdf, svg, txt.

#### `gp:end-data`

```
[procedure] (end-data)
```

Send end data signal `e` to Gnuplot pipe and flush output.
