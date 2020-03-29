(import (prefix (gnuplot-pipe plot) gp:))

;;; Plot function
(gp:call/gnuplot
 (gp:send "set xlabel 'x'")
 (gp:send "plot sin(x)"))

;;; Plot x and y axes data as lines
(gp:call/gnuplot
 (gp:plot "title 'x^2'" '(1 2 3 4) '(1 4 9 16)))

;;; Plot multiple curves
(gp:call/gnuplot
 (gp:plot '(("title 'x'" (1 2 3 4) (1 2 3 4))
            ("title 'x^2'" (1 2 3 4) (1 4 9 16))
            ("title 'x^3'" (1 2 3 4) (1 8 27 64)))))

;;; 3d plot
(gp:call/gnuplot
 (gp:send "unset key")
 (gp:send "set style data points")
 (gp:send "set title 'The valley of the Gnu'")
 (gp:plot3d '((""
               (0 0 0 1 1 1 2 2 2 3 3 3)
               (0 1 2 0 1 2 0 1 2 0 1 2)
               (10 10 10 10 5 10 10 1 10 10 0 10)))))

(gp:call/gnuplot
 (gp:send "set style data points")
 (gp:send "set title 'The valley of the Gnu'")
 (gp:plot3d '(("title 'series 1'"
               (0 0 0 1 1 1 2 2 2 3 3 3)
               (0 1 2 0 1 2 0 1 2 0 1 2)
               (9 9 9 9 4 9 9 0 9 9 1 9))
              ("title 'series 2'"
               (0 0 0 1 1 1 2 2 2 3 3 3)
               (0 1 2 0 1 2 0 1 2 0 1 2)
               (10 10 10 10 5 10 10 1 10 10 0 10)))))

;;; Pie chart
(gp:call/gnuplot
 (gp:send "set xrange [-15:15]")
 (gp:send "set style fill transparent solid 0.9 noborder")
 (gp:plot "using 1:2:3:4:5:6 with circles lc var"
          '(0 0 0 0 0) '(0 0 0 0 0) '(5 5 5 5 5)
          '(0 30 70 120 230) '(30 70 120 230 360) '(1 2 3 4 5)))

;;; Matrix
(gp:call/gnuplot
 (gp:plot "matrix with image"
          '(5 2 0 0) '(4 2 0 1) '(3 0 0 2) '(1 0 1 4) '(0 1 0 3))
 (gp:end-data))

