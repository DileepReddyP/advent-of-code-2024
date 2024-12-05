(define-module (aoc-2024-common)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 textual-ports)
  #:export (create-grid-dict
            list-combinations
            array-ref-safe
            possible-grid-directions))

(define (create-grid-dict input-string)
  (let ([grid-dict (make-hash-table)]
        [i 0])
    (string-for-each-index
     (lambda (j)
       (if (char=? (string-ref input-string j) #\newline)
           (set! i (1+ i))
           (hash-set! grid-dict (make-rectangular i j) (string-ref input-string j))))
     input-string)
    grid-dict))

(define (list-combinations report)
  (map (lambda (i)
         (append (list-head report i) (list-tail report (1+ i))))
       (iota (length report))))

(define possible-grid-directions '(1 0+1i 1+1i 1-1i -1 -1i -1+1i -1-1i))

(define (array-ref-safe arr i j)
  (and (array-in-bounds? arr i j)
       (array-ref arr i j)))
