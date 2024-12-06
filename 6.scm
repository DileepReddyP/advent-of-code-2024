(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-26)
             (srfi srfi-43)
             (srfi srfi-71)
             (srfi srfi-171)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define example-data
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
")

(define part-6-data
  (call-with-input-file "./6.txt" get-string-all))

(define directions
  '((up . (-1 . 0))
    (left . (0 . -1))
    (down . (1 . 0))
    (right . (0 . 1))))

(define 90-degree-turn
  '((up . right)
    (right . down)
    (down . left)
    (left . up)))

(define (parse-data data)
  (let* ([list-of-strings (string-split (substring data 0 (1- (string-length data)))
                                        #\newline)]
         [start-x '()]
         [start-y '()]
         [lab-array (list-transduce
                     (compose (tenumerate 0)
                              (tmap (match-lambda
                                      [(i . row-string)
                                       (let ([row-list '()])
                                         (string-for-each-index
                                          (lambda (j)
                                            (let ([this-char (string-ref row-string j)])
                                              (when (equal? this-char  #\^)
                                                (set! start-x i)
                                                (set! start-y j))
                                              (set! row-list (cons this-char row-list))))
                                          row-string)
                                         row-list)]))
                              (tmap reverse))
                     rcons
                     list-of-strings)])
    (values (list->array 2 lab-array) start-x start-y)))

(define (guard-path x y direction lab-array unique-steps)
  (begin
    (when (not (hash-ref unique-steps `(,x . ,y)))
      (hash-set! unique-steps `(,x . ,y) #t))
    (let* ([dir-x (car (assoc-ref directions direction))]
           [dir-y (cdr (assoc-ref directions direction))]
           [next-x (+ x dir-x)]
           [next-y (+ y dir-y)])
      (match (array-ref-safe lab-array next-x next-y)
        [#f
         unique-steps]
        [#\#
         (match-let* ([new-dir (assoc-ref 90-degree-turn direction)]
                      [(new-dx . new-dy) (assoc-ref directions new-dir)]
                      [new-x (+ x new-dx)]
                      [new-y (+ y new-dy)])
           (guard-path
            new-x new-y new-dir lab-array unique-steps))]
        [_
         (guard-path
          next-x next-y direction lab-array unique-steps)]))))

(define (guard-path-loop-detect
         x y direction lab-array unique-steps-with-dir)
  (let* ([dir-x (car (assoc-ref directions direction))]
         [dir-y (cdr (assoc-ref directions direction))]
         [next-x (+ x dir-x)]
         [next-y (+ y dir-y)])
    (if (hash-ref unique-steps-with-dir `((,x . ,y) . ,direction))
        #t
        (begin
          (hash-set! unique-steps-with-dir `((,x . ,y) . ,direction) #t)
          (match (array-ref-safe lab-array next-x next-y)
            [#f #f]
            [#\#
             (let* ([new-dir (assoc-ref 90-degree-turn direction)]
                    [new-dx (car (assoc-ref directions new-dir))]
                    [new-x (+ x new-dx)]
                    [new-dy (cdr (assoc-ref directions new-dir))]
                    [new-y (+ y new-dy)])
               (begin
                 ;; corner check
                 (while (equal? #\# (array-ref-safe lab-array new-x new-y))
                   (set! new-dir (assoc-ref 90-degree-turn new-dir))
                   (set! new-dx (car (assoc-ref directions new-dir)))
                   (set! new-x (+ x new-dx))
                   (set! new-dy (cdr (assoc-ref directions new-dir)))
                   (set! new-y (+ y new-dy)))
                 (guard-path-loop-detect
                  new-x new-y new-dir lab-array unique-steps-with-dir)))]
            [_
             (guard-path-loop-detect
              next-x next-y direction lab-array unique-steps-with-dir)])))))

(define (solve-6 dataset)
  (statprof
   (lambda ()
     (let* ([lab-array start-x start-y (parse-data dataset)]
            [unique-steps (make-hash-table)]
            [no-of-blocks 0])
       (begin
         (guard-path
          start-x start-y 'up lab-array unique-steps)
         (hash-for-each
          (lambda (k v)
            (match-let ([(i . j) k]
                        [new-lab-array (apply (cut make-array #f <> <>)
                                              (array-dimensions lab-array))]
                        [unique-steps-with-dir (make-hash-table)])
              (array-copy! lab-array new-lab-array)
              (array-set! new-lab-array #\# i j)
              (hash-set! unique-steps-with-dir `((,start-x . ,start-y) . 'up) #t)
              (if (guard-path-loop-detect
                   start-x start-y 'up new-lab-array unique-steps-with-dir)
                  (set! no-of-blocks (1+ no-of-blocks)))))
          unique-steps)
         (values (hash-count (const #t) unique-steps)
                 no-of-blocks))))))

