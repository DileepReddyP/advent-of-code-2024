(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-26)
             (srfi srfi-41)
             (srfi srfi-43)
             (srfi srfi-71)
             (srfi srfi-171)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define example-data
  "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
")

(define part-8-data
  (call-with-input-file "./8.txt" get-string-all))

(define (parse-data data)
  (create-grid-dict data))

(define (find-antinodes grid-dict part-2?)
  (let ([grid-dict-reversed (make-hash-table)]
        [unique-antinodes (make-hash-table)])
    (hash-for-each
     (lambda (co-ord antenna)
       (when (not (char=? antenna #\.))
         (let ([antenna-coord-list (hash-ref grid-dict-reversed antenna)])
           (if (not antenna-coord-list)
               (hash-set! grid-dict-reversed antenna (list co-ord))
               (begin
                 (for-each
                  (lambda (prev-coord)
                    (let* ([distance (- co-ord prev-coord)]
                           [antinode-1 (lambda (x) (+ co-ord (* x distance)))]
                           [antinode-2 (lambda (x) (- prev-coord (* x distance)))])
                      (stream-for-each
                       (lambda (x)
                         (when (and (hash-ref grid-dict (antinode-1 x))
                                    (not (hash-ref unique-antinodes (antinode-1 x))))
                           (hash-set! unique-antinodes (antinode-1 x) #t))
                         (when (and (hash-ref grid-dict (antinode-2 x))
                                    (not (hash-ref unique-antinodes (antinode-2 x))))
                           (hash-set! unique-antinodes (antinode-2 x) #t)))
                       (stream-take-while (lambda (x)
                                            (or (hash-ref grid-dict (antinode-1 x))
                                                (hash-ref grid-dict (antinode-2 x))))
                                          (if part-2?
                                              (stream-from 0)
                                              (list->stream '(1)))))))
                  antenna-coord-list)
                 (hash-set! grid-dict-reversed antenna (cons co-ord antenna-coord-list)))))))
     grid-dict)
    unique-antinodes))

(define (solve-8 dataset)
  (statprof
   (lambda ()
     (let* ([grid-dict (parse-data dataset)]
            [antinode-grid-1 (find-antinodes grid-dict #f)]
            [antinode-grid-2 (find-antinodes grid-dict #t)])
       (values (hash-count (const #t) antinode-grid-1)
               (hash-count (const #t) antinode-grid-2))))))

