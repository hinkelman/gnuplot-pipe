#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; SPDX-License-Identifier: GPL-3.0-or-later
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (prefix (gnuplot-pipe) gp:))

;;; macro included in the chicken version of this file wasn't working
;;; because the test to check if the files existed was happening before the files were written
;;; added a sleep timer to give the files a chance to be written before the tests are run

;;; Plot function.
(gp:call/gnuplot
 (gp:send "set xlabel 'x'")
 (gp:send "plot sin(x)")
 (gp:save "plot1.png"))

;;; Plot x and y axes data as lines.
(gp:call/gnuplot
 (gp:plot "title 'x^2'" '(1 2 3 4) '(1 4 9 16))
 (gp:save "plot2.png"))

;;; Plot multiple curves.
(gp:call/gnuplot
 (gp:plot '(("title 'x^2'" (1 2 3 4) (1 4 9 16))))
 (gp:save "plot3.png"))

(gp:call/gnuplot
 (gp:plot '(("title 'x^2'" (1 2 3 4) (1 4 9 16))
            ("title 'x^3'" (1 2 3 4) (1 8 27 64))))
 (gp:save "plot4.png"))

(gp:call/gnuplot
 (gp:plot '(("title 'x'" (1 2 3 4) (1 2 3 4))
            ("title 'x^2'" (1 2 3 4) (1 4 9 16))
            ("title 'x^3'" (1 2 3 4) (1 8 27 64))))
 (gp:save "plot5.png"))

;;; 3d plot.
(gp:call/gnuplot
 (gp:send "unset key")
 (gp:send "set style data points")
 (gp:send "set title 'The valley of the Gnu'")
 (gp:plot3d ""
            '(0 0 0 1 1 1 2 2 2 3 3 3)
            '(0 1 2 0 1 2 0 1 2 0 1 2)
            '(10 10 10 10 5 10 10 1 10 10 0 10))
 (gp:save "plot6.png"))

(gp:call/gnuplot
 (gp:send "unset key")
 (gp:send "set style data points")
 (gp:send "set title 'The valley of the Gnu'")
 (gp:plot3d '((""
               (0 0 0 1 1 1 2 2 2 3 3 3)
               (0 1 2 0 1 2 0 1 2 0 1 2)
               (10 10 10 10 5 10 10 1 10 10 0 10))))
 (gp:save "plot7.png"))

(gp:call/gnuplot
 (gp:send "set style data points")
 (gp:send "set title 'The valley of the Gnu'")
 (gp:plot3d '(("title 'series 1'"
               (0 0 0 1 1 1 2 2 2 3 3 3)
               (0 1 2 0 1 2 0 1 2 0 1 2)
               (10 10 10 10 5 10 10 1 10 10 0 10))
              ("title 'series 2'"
               (0 0 0 1 1 1 2 2 2 3 3 3)
               (0 1 2 0 1 2 0 1 2 0 1 2)
               (10 10 10 10 5 10 10 1 10 10 0 10))))
 (gp:save "plot8.png"))

;;; Pie chart.
(gp:call/gnuplot
 (gp:send "set xrange [-15:15]")
 (gp:send "set style fill transparent solid 0.9 noborder")
 (gp:plot "using 1:2:3:4:5:6 with circles lc var"
          '(0 0 0 0 0) '(0 0 0 0 0) '(5 5 5 5 5)
          '(0 30 70 120 230) '(30 70 120 230 360) '(1 2 3 4 5))
 (gp:save "plot9.png"))

;;; Matrix
(gp:call/gnuplot
 (gp:plot "matrix with image"
          '(5 2 0 0) '(4 2 0 1) '(3 0 0 2) '(1 0 1 4) '(0 1 0 3))
 (gp:end-data)
 (gp:save "plot10.png"))

;;; need to give time for files to be written before checking for existence of files
(sleep (make-time 'time-duration 0 5))

(test-begin "gnuplot-test")

(define filenames
  (map (lambda (x) (string-append "plot" (number->string (add1 x)) ".png")) (iota 10)))

(for-each (lambda (x) (test-assert (file-exists? x))) filenames)

(for-each delete-file filenames) 

(test-end "gnuplot-test")

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
