(define-module (aoc-2024-common)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 textual-ports)
  #:export (create-grid-dict
            grid-dict-ref
            grid-directions
            grid-directions-with-diagonals
            copy-hash-table
            char->number
            list-combinations
            array-ref-safe
            list-permutations-with-repeats))

;; (srfi srfi-1) lists
;; (srfi srfi-9) records
;; (srfi srfi-26) cut
;; (srfi srfi-41) streams
;; (srfi srfi-42) eager comprehensions
;; (srfi srfi-43) vectors
;; (srfi srfi-71) let multi-value bindings
;; (srfi srfi-171) transducers

(define (create-grid-dict input-string)
  (let ([grid-dict (make-hash-table)]
        [string-list (remove string-null? (string-split input-string #\newline))])
    (do-ec (:list s (index i) string-list)
           (:string c (index j) s)
           (hash-set! grid-dict
                      (make-rectangular (exact->inexact i) (exact->inexact j))
                      c))
    grid-dict))

(define (grid-dict-ref grid-dict i j)
  (hash-ref grid-dict (make-rectangular i j)))

(define grid-directions-with-diagonals
  '(1.0+0.0i 1.0+1.0i 0.0+1.0i -1.0+1.0i -1.0+0.0i -1.0-1.0i 0.0-1.0i 1.0-1.0i))

(define grid-directions
  '(1.0+0.0i 0.0+1.0i -1.0+0.0i 0.0-1.0i))

(define (complex< a b)
  (let ([ra ia (values (real-part a) (imag-part a))]
        [rb ib (values (real-part b) (imag-part b))])
    (or (< ra rb)
        (and (= ra rb) (< ia ib)))))

(define (array-ref-safe arr i j)
  (and (array-in-bounds? arr i j)
       (array-ref arr i j)))

(define (copy-hash-table hash-table)
  (alist->hash-table (hash-map->list cons hash-table)))

(define (char->number c)
  (- (char->integer c) 48))

(define (list-combinations report)
  (map (lambda (i)
         (append (list-head report i) (list-tail report (1+ i))))
       (iota (length report))))

(define (list-permutations-with-repeats lst n)
  (match n
    [0 '(())]
    [n (apply append (map (lambda (p)
                            (map (cut cons <> p) lst))
                          (list-permutations-with-repeats lst (1- n))))]))

(define (cache-proc proc)
  (let ([cache (make-hash-table)])
    (lambda args
      (let ([cached (hash-ref cache args)])
        (if (not cached)
            (begin
              (let ([result (apply proc args)])
                (hash-set! cache args result)
                result))
            cached)))))

;; example :of do generator
(define (fibonacci n)
  (list-ec (:do ([i 0] [a 0] [b 1]) ; bindings
                (< i n)             ; condition
                ((1+ i) b (+ a b))) ; re-binding
           a))
