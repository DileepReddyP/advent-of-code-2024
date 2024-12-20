(use-modules (statprof)
             (ice-9 q)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define *example-data*
  "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
")

(define *part-20-data*
  (call-with-input-file "./20.txt" get-string-all))

(define (race-start-and-end race-map)
  (values (hash-fold (lambda (p c prev)
                       (if (char=? c #\S) p prev))
                     '() race-map)
          (hash-fold (lambda (p c prev)
                       (if (char=? c #\E) p prev))
                     '() race-map)))

(define (manhattan- z1 z2)
  (let ([delta (- z1 z2)])
    (+ (abs (real-part delta))
       (abs (imag-part delta)))))

(define (race-bfs race-map start-pos end-pos)
  (let ([visited (make-hash-table)]
        [queue (make-q)])
    (hash-set! visited start-pos #t)
    (set! queue (enq! queue `(,start-pos . ())))
    (let loop ([bq queue])
      (if (positive? (q-length bq))
          (let* ([current-pos path (car+cdr (deq! bq))]
                 [next-path (cons current-pos path)])
            (if (= current-pos end-pos)
                next-path
                (begin
                  (do-ec (:list d grid-directions)
                         (:let next-pos (+ current-pos d))
                         (:let next-char (hash-ref race-map next-pos))
                         (and next-char
                              (not (hash-ref visited next-pos))
                              (not (char=? next-char #\#)))
                         (begin
                           (hash-set! visited next-pos #t)
                           (set! bq (enq! bq `(,next-pos . ,next-path)))))
                  (loop bq))))
          #f))))

(define (count-cheats path-list min-save max-cheat)
  (sum-ec (:list pos (index i) path-list)
          (:list next (index j) (list-tail path-list (1+ i)))
          (:let c (1+ j))
          (:let dist (manhattan- pos next))
          (and (<= dist max-cheat)
               (<= min-save (- c dist)))
          1))

(define (solve-20 data)
  (statprof
   (lambda ()
     (let* ([race-map (create-grid-dict data)]
            [start-pos end-pos (race-start-and-end race-map)]
            [standard-path (race-bfs race-map start-pos end-pos)])
       (values (count-cheats standard-path 100 2)
               (count-cheats standard-path 100 20))))))
