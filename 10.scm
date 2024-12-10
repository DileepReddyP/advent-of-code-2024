(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define part-10-data
  (call-with-input-file "./10.txt" get-string-all))

(define example-data
  "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
")

(define (parse-data dataset)
  (let ([topo-map (create-grid-dict dataset)]
        [trailheads '()])
    (hash-for-each
     (lambda (k c)
       (let ([v (char->number c)])
         (hash-set! topo-map k v)
         (if (zero? v)
             (set! trailheads (cons k trailheads)))))
     topo-map)
    (values trailheads topo-map)))

(define* (trail-search topo-map co-ord current-elevation #:optional visited)
  (cond
   [(and visited (hash-ref visited co-ord)) 0]
   [(= current-elevation 9) (begin
                              (and visited (hash-set! visited co-ord #t))
                              1)]
   [else (let ([candidate-list (map (cut + co-ord <>) grid-directions)]
               [next-elevation (1+ current-elevation)])
           (and visited (hash-set! visited co-ord #t))
           (sum-ec (:list c candidate-list)
                   (:let ce (hash-ref topo-map c))
                   (and ce (= ce next-elevation))
                   (trail-search topo-map c next-elevation visited)))]))

(define (solve-10 dataset)
  (statprof
   (lambda ()
     (let ([trailheads topo-map (parse-data dataset)])
       (values (sum-ec (:list co-ord trailheads)
                       (trail-search topo-map co-ord 0 (make-hash-table)))
               (sum-ec (:list co-ord trailheads)
                       (trail-search topo-map co-ord 0)))))))
