(library (gnuplot-pipe)
  (export
   call/gnuplot
   send
   plot
   plot3d
   save
   end-data)

  (import (chezscheme)
          (only (srfi :13 strings) string-join)
          (only (srfi :1 lists) fold))

  ;;---------------------------------------------------------------------

  ;; Public procedures and macros.

  (define-syntax call/gnuplot
    (syntax-rules ()
      ((_ expr ...)
       (with-output-to-pipe "gnuplot --persist"
                            (lambda ()
                              (send "set style data lines")
                              expr ...
                              (flush-output-port)
                              (newline)
                              (send "quit"))))))

  ;; Send arbitrary command to Gnuplot.
  (define (send . cmdline)
    (for-each (lambda (arg) (display arg))
              cmdline)
    (newline)
    (flush-output-port))

  ;; Plot inline data.
  (define (plot . element)
    (plot-generic "plot" element "(plot element)"))

  ;; 3d plot inline data.
  (define (plot3d . element)
    (plot-generic "splot" element "(plot3d element)"))

  ;; Save last plot to file.
  (define (save fname)
    (define ext (path-extension fname))
    (cond [(not ext)
           (send "set terminal png")]
          [(or (equal? ext "pdf") (equal? ext "png") (equal? ext "svg"))
           (send (format "set terminal ~A" ext))]
          [(equal? ext "txt")
           (send "set terminal dumb")]
          [else
           (assertion-violation "(save fname)" "Extension must be png, pdf, svg or txt")])
    (send "set output")
    (send (format "set output '~A'" fname))
    (send "replot")
    (send "unset output")
    (send "unset terminal"))

  (define (end-data)
    (display "e")
    (newline)
    (flush-output-port))

  ;;---------------------------------------------------------------------

  ;; Internal procedures.

  ;; (with-output-to-pipe) contributed by oaktownsam on Scheme discord server
  (define (with-output-to-pipe cmdline thunk)
    (define-values (to-cmd from-cmd from-cmd-err cmd-pid) 
      (open-process-ports cmdline 'line (native-transcoder)))
    (dynamic-wind
      void
      (lambda ()
        (parameterize ((current-output-port to-cmd))
          (thunk)))
      (lambda ()
        (close-output-port to-cmd)
        (close-input-port from-cmd)
        (close-input-port from-cmd-err))))

  (define (plot-row . row)
    (display (string-join (map number->string row)))
    (newline))

  (define (dataset-same-length? data)
    (apply = (map length data)))

  (define (plot-dataset-single data who)
    (cond [(and (list? data)           ; '((1 2 3 4) (10 20 30 40) ...)
                (list? (car data))
                (number? (caar data)))
           (unless (dataset-same-length? data)
             (assertion-violation who "Data rows of different length"))
           (apply for-each plot-row data)]
          [else
           (assertion-violation who "Invalid data set")])
    (end-data))

  (define (plot-dataset data who)
    (for-each (lambda (x) (plot-dataset-single x who)) data))

  ;; Plot using cmdline the list of descriptions and wait for data input.
  (define (newplot cmdline description)
    (apply send
           cmdline " '-' " (car description)
           (string-join (cdr description) ", '-' " 'prefix)
           '()))

  ;; Extract a list of descriptions from a list of plot elements.
  (define (plot-element-description element)
    (map car element))

  ;; Extract a list of data from a list of plot elements.
  (define (plot-element-data element)
    (map cdr element))

  ;; Are all element descriptions strings?
  (define (element-description-string? element)
    (fold (lambda (x y) (and x y))
          #t
          (map string? (plot-element-description element))))

  ;; Dispatch according to arguments format.
  (define (plot-generic cmdline element who)
    (let ((plot-element (cond [(null? element)
                               (assertion-violation who "No plot element specified")]
                              [(string? (car element))
                               (list element)] ; ((str lst1 lst2 ...))
                              [(element-description-string? (car element))
                               (car element)] ; ((str l1st lst2 ...) ...)
                              [else
                               (assertion-violation who "Plot element format not recognized")])))
      (newplot cmdline (plot-element-description plot-element))
      (plot-dataset (plot-element-data plot-element) who)))

  )

