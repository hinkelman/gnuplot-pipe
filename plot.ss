(library (gnuplot-pipe plot)
  (export
   call/gnuplot
   send
   plot
   plot3d
   save
   end-data)

  (import (chezscheme)
          (only (srfi s13 strings) string-join)
          (only (srfi s1 lists) fold))

  ;; At this point, all I have done is paste the Chicken code into this file,
  ;;  which is formatted as a Chez library, add SRFIs 1 & 13 as a dependencies,
  ;;  and changed all the instances of (flush-output) to (flush-output-port)

  ;; Need to write procedure to replace (pathname-extension), which returns
  ;;  the file extension when passed a pathname string

  ;; Need to write procedure to replace (with-output-to-pipe) in (call/gnuplot) macro

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
    (plot-generic "plot" element))

  ;; 3d plot inline data.
  (define (plot3d . element)
    (plot-generic "splot" element))

  ;; Save last plot to file.
  (define (save fname)
    (define ext (pathname-extension fname))
    (cond ((not ext)
           (send "set terminal png"))
          ((or (equal? ext "pdf") (equal? ext "png") (equal? ext "svg"))
           (send (format "set terminal ~A" ext)))
          ((equal? ext "txt")
           (send "set terminal dumb"))
          (else
           (error "Extension must be png, pdf, svg or txt")))
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

  (define (plot-row . row)
    (display (string-join (map number->string row)))
    (newline))

  (define (dataset-same-length? data)
    (apply = (map length data)))

  (define (plot-dataset-single data)
    (cond ((and (list? data)           ; '((1 2 3 4) (10 20 30 40) ...)
                (list? (car data))
                (number? (caar data)))
           (unless (dataset-same-length? data)
             (error "Data rows of different length"))
           (apply for-each plot-row data))
          (else
           (error "Invalid data set ")))
    (end-data))

  (define (plot-dataset data)
    (map plot-dataset-single data))

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
  (define (plot-generic cmdline element)
    (let ((plot-element (cond ((null? element)
                               (error "No plot element specified"))
                              ((string? (car element))
                               (list element)) ; ((str lst1 lst2 ...))
                              ((element-description-string? (car element))
                               (car element)) ; ((str l1st lst2 ...) ...)
                              (else
                               (error "Plot element format not recognized")))))
      (newplot cmdline (plot-element-description plot-element))
      (plot-dataset (plot-element-data plot-element))))

  )

