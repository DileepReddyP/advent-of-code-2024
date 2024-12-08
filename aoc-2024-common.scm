(define-module (aoc-2024-common)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 textual-ports)
  #:export (create-grid-dict
            grid-dict-ref
            possible-grid-directions
            list-combinations
            array-ref-safe
            list-permutations-with-repeats))

(define (create-grid-dict input-string)
  (let ([grid-dict (make-hash-table)]
        [string-list (remove string-null? (string-split input-string #\newline))])
    (for-each
     (lambda (i)
       (let ([this-string (list-ref string-list i)])
         (string-for-each-index
          (lambda (j)
            (hash-set! grid-dict
                       (make-rectangular (exact->inexact i) (exact->inexact j))
                       (string-ref this-string j)))
          this-string)))
     (iota (length string-list)))
    grid-dict))

(define (grid-dict-ref grid-dict i j)
  (hash-ref grid-dict (make-rectangular i j)))

(define (list-combinations report)
  (map (lambda (i)
         (append (list-head report i) (list-tail report (1+ i))))
       (iota (length report))))

(define possible-grid-directions
  (circular-list 1 0+1i 1+1i 1-1i -1 -1i -1+1i -1-1i))

(define (array-ref-safe arr i j)
  (and (array-in-bounds? arr i j)
       (array-ref arr i j)))

(define (list-permutations-with-repeats lst n)
  (match n
    [0 '(())]
    [n (apply append (map (lambda (p)
                            (map (cut cons <> p) lst))
                          (list-permutations-with-repeats lst (1- n))))]))
