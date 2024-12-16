(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-2)
             (srfi srfi-9)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define *example-data*
  "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
")

(define *part-16-data*
  (call-with-input-file "./16.txt" get-string-all))

(define (maze-start-and-end maze-map)
  (values (hash-fold (lambda (p c prev)
                       (if (char=? c #\S) p prev))
                     '() maze-map)
          (hash-fold (lambda (p c prev)
                       (if (char=? c #\E) p prev))
                     '() maze-map)))

(define (parse-data data)
  (create-grid-dict data))

(define direction-words
  '(south east north west))

(define direction-alist
  (map cons direction-words grid-directions))

(define possible-turns
  '((south . (south east west))
    (east . (east north south))
    (north . (north east west))
    (west . (west north south))))

(define (manhattan- z1 z2)
  (+ (abs (- (real-part z1) (real-part z2)))
     (abs (- (imag-part z1) (imag-part z2)))))

(define-record-type <pq-node>
  (make-pq-node pos direction f-score predecessors)
  pq-node?
  (pos pq-pos)
  (direction pq-dir)
  (f-score pq-f-score)
  (predecessors pq-predecessors))

(define node-cost<
  (match-lambda*
    [(($ <pq-node> _ _ c1 _) ($ <pq-node> _ _ c2 _)) (< c1 c2)]))

(define (deer-A* maze-map start-pos end-pos)
  (let ([g-score (make-hash-table)]
        [min-cost (inf)]
        [unique-paths '()])
    (hash-set! g-score `(,start-pos . east) 0)
    (let loop ([pq `(,(make-pq-node start-pos 'east (manhattan- start-pos end-pos) '()))])
      (when (positive? (length pq))
        (let* ([current rest (car+cdr (sort! pq node-cost<))]
               [current-pos (pq-pos current)]
               [current-cost (pq-f-score current)]
               [current-dir (pq-dir current)]
               [current-cons `(,current-pos . ,current-dir)]
               [current-predecessors (pq-predecessors current)]
               [current-g-score (hash-ref g-score current-cons (inf))]
               [next-predecessors (cons current-pos current-predecessors)]
               [turns (assoc-ref possible-turns current-dir)])
          (when (<= current-cost min-cost)
            (when (= current-pos end-pos)
              (set! min-cost current-cost)
              (set! unique-paths (cons next-predecessors unique-paths))
              (loop rest))
            (do-ec (:list t turns)
                   (:let d (assoc-ref direction-alist t))
                   (:let next-pos (+ current-pos d))
                   (:let next-cons `(,next-pos . ,t))
                   (:let tent-g-score (+ current-g-score
                                         (if (eqv? t current-dir) 1 1001)))
                   (and (not (char=? (hash-ref maze-map next-pos) #\#))
                        (<= tent-g-score (hash-ref g-score next-cons (inf))))
                   (begin
                     (hash-set! g-score next-cons tent-g-score)
                     (set! rest (cons (make-pq-node next-pos
                                                    t
                                                    (+ tent-g-score
                                                       (manhattan- next-pos
                                                                   end-pos))
                                                    next-predecessors)
                                      rest))))
            (loop rest)))))
    (values min-cost (count-common unique-paths))))

(define (count-common unique-tiles)
  (length (apply (cut lset-union = <...>) unique-tiles)))

(define (solve-16 dataset)
  (statprof
   (lambda ()
     (let* ([maze-map (parse-data dataset)]
            [start-pos end-pos (maze-start-and-end maze-map)])
       (deer-A* maze-map start-pos end-pos)))))
