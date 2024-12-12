(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define part-12-data
  (call-with-input-file "./12.txt" get-string-all))

(define example-data
  "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
")

(define (parse-data dataset)
  (create-grid-dict dataset))

(define (perimeter-scan plot-map co-ord current-plant visited)
  (let ([p (hash-ref plot-map co-ord)])
    (cond
     [(hash-ref visited co-ord) 0]
     [(or (not p) (not (char=? current-plant p))) 1]
     [else (let ([candidate-coords (map (cut + co-ord <>) grid-directions)]
                 [perimeter 0])
             (hash-set! visited co-ord #t)
             (hash-set! plot-map co-ord #\.)
             (do-ec (:list next-coord candidate-coords)
                    (set! perimeter (+ perimeter (perimeter-scan plot-map next-coord
                                                                 current-plant visited))))
             perimeter)])))

(define (count-sides ct)
  (hash-fold
   (lambda (c _ sides/corners)
     (match-let ([(s sw w nw n ne e se)
                  (map (lambda (d) (hash-ref ct (+ c d)))
                       grid-directions-with-diagonals)])
       (+ sides/corners
          (if (or (and (not n) (not e)) (and n e (not ne))) 1 0)
          (if (or (and (not n) (not w)) (and n w (not nw))) 1 0)
          (if (or (and (not s) (not e)) (and s e (not se))) 1 0)
          (if (or (and (not s) (not w)) (and s w (not sw))) 1 0))))
   0 ct))

(define (fence-price plot-map part2?)
  (hash-fold (lambda (co-ord plant prev)
               (if (char=? plant #\.)
                   prev
                   (let* ([ct (make-hash-table)]
                          [p (perimeter-scan plot-map co-ord plant ct)]
                          [a (hash-count (const #t) ct)])
                     (+ prev (* a (if part2? (count-sides ct) p))))))
             0 plot-map))

(define (solve-12.1 dataset)
  (statprof
   (lambda ()
     (let ([plot-map (parse-data dataset)])
       (fence-price plot-map #f)))))

(define (solve-12.2 dataset)
  (statprof
   (lambda ()
     (let ([plot-map (parse-data dataset)])
       (fence-price plot-map #t)))))

