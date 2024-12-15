(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-2)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define *example-data*
  "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
")

(define *part-15-data*
  (call-with-input-file "./15.txt" get-string-all))

(define-peg-pattern dataset body
  (and (+ warehouse) nl moveset))
(define-peg-pattern warehouse body
  (and (+ (or "#" "." "@" "O")) "\n"))
(define-peg-pattern moveset body
  (+ (and (+ (or l-move r-move d-move u-move)) nl)))
(define-peg-pattern u-move all
  (ignore "^"))
(define-peg-pattern d-move all
  (ignore "v"))
(define-peg-pattern r-move all
  (ignore ">"))
(define-peg-pattern l-move all
  (ignore "<"))
(define-peg-pattern nl none
  "\n")

(define direction-list
  (map cons '(d-move r-move u-move l-move) grid-directions))

(define (robot-start warehouse-map)
  (hash-fold (lambda (p c prev)
               (if (char=? c #\@) p prev))
             '() warehouse-map))

(define (parse-data data)
  (let* ([map-str p-moves (car+cdr (peg:tree (match-pattern dataset data)))]
         [parsed-moves (first p-moves)]
         [warehouse-map (create-grid-dict map-str)]
         [movelist (if (list? (first parsed-moves))
                       (apply append parsed-moves) parsed-moves)])
    (values warehouse-map movelist)))

(define (one-move.1 warehouse-map p dp)
  (let ([next-pos (+ p dp)])
   (match (hash-ref warehouse-map next-pos)
     [#\# #f]
     [#\. (list next-pos)]
     [#\O (and=> (one-move.1 warehouse-map next-pos dp) (cut cons next-pos <>))])))

(define (robot-in-motion.1 robot-init-pos warehouse-map robo-movelist)
  (let ([p robot-init-pos])
    (do-ec (:list m robo-movelist)
           (:let dp (assoc-ref direction-list m))
           (:let affected-cells (one-move.1 warehouse-map p dp))
           (if affected-cells)
           (let ([next-pos box-pos (car+cdr affected-cells)])
             (hash-set! warehouse-map p #\.)
             (hash-set! warehouse-map next-pos #\@)
             (for-each (cut hash-set! warehouse-map <> #\O) box-pos)
             (set! p next-pos)))
    warehouse-map))

(define (solve-15.1 dataset)
  (statprof
   (lambda ()
     (let* ([warehouse-map robo-movelist (parse-data dataset)]
            [robot-init-pos (robot-start warehouse-map)]
            [after-moves (robot-in-motion.1 robot-init-pos warehouse-map robo-movelist)])
       (hash-fold (lambda (p c prev)
                    (if (char=? c #\O)
                        (+ prev (* 100 (real-part p)) (* 1 (imag-part p)))
                        prev))
                  0 after-moves)))))

(define (widen-map old-map)
  (let ([new-map (make-hash-table)]
        [delta 0.0+1.0i]
        [i2 0.0+2.0i])
    (hash-for-each
     (lambda (p c)
       (match c
         [#\. (and (hash-set! new-map
                              (+ (real-part p) (* i2 (imag-part p)))
                              #\.)
                   (hash-set! new-map
                              (+ (real-part p) (* i2 (imag-part p)) delta)
                              #\.))]
         [#\O (and (hash-set! new-map
                              (+ (real-part p) (* i2 (imag-part p)))
                              #\[)
                   (hash-set! new-map
                              (+ (real-part p) (* i2 (imag-part p)) delta)
                              #\]))]
         [#\@ (and (hash-set! new-map
                              (+ (real-part p) (* i2 (imag-part p)))
                              #\@)
                   (hash-set! new-map
                              (+ (real-part p) (* i2 (imag-part p)) delta)
                              #\.))]
         [#\# (and (hash-set! new-map
                              (+ (real-part p) (* i2 (imag-part p)))
                              #\#)
                   (hash-set! new-map
                              (+ (real-part p) (* i2 (imag-part p)) delta)
                              #\#))]))
     old-map)
    new-map))

(define* (one-move.2 warehouse-map p dp move)
  (let ([next-pos (+ p dp)])
   (match (hash-ref warehouse-map next-pos)
     [#\# #f]
     [#\. (list `(,next-pos . #\.))]
     [#\[ (if (or (eqv? move 'l-move) (eqv? move 'r-move))
              (and=> (one-move.2 warehouse-map next-pos dp move)
                     (cut cons `(,next-pos . #\[) <>))
              (and-let* ([r-box-pos (+ next-pos 0.0+1.0i)]
                         [l-check (one-move.2 warehouse-map next-pos dp move)]
                         [r-check (one-move.2 warehouse-map r-box-pos dp move)])
                (append `((,next-pos . #\[) (,r-box-pos . #\])) l-check r-check)))]
     [#\] (if (or (eqv? move 'l-move) (eqv? move 'r-move))
              (and=> (one-move.2 warehouse-map next-pos dp move)
                     (cut cons `(,next-pos . #\]) <>))
              (and-let* ([l-box-pos (- next-pos 0.0+1.0i)]
                         [l-check (one-move.2 warehouse-map l-box-pos dp move)]
                         [r-check (one-move.2 warehouse-map next-pos dp move)])
                (append `((,l-box-pos . #\[) (,next-pos . #\])) l-check r-check)))])))

(define (robot-in-motion.2 robot-init-pos warehouse-map robo-movelist)
  (let ([p robot-init-pos])
    (do-ec (:list m robo-movelist)
           (:let dp (assoc-ref direction-list m))
           (:let affected-cells (one-move.2 warehouse-map p dp m))
           (if affected-cells)
           (let ([next-pos (+ p dp)])
             (for-each (match-lambda
                         [(ap . _) (hash-set! warehouse-map ap #\.)])
                       affected-cells)
             (for-each (match-lambda
                         [(ap . ac) (when (not (char=? ac #\.))
                                      (hash-set! warehouse-map (+ ap dp) ac))])
                       affected-cells)
             (hash-set! warehouse-map p #\.)
             (hash-set! warehouse-map next-pos #\@)
             (set! p next-pos)))
    warehouse-map))

(define (solve-15.2 dataset)
  (statprof
   (lambda ()
     (let* ([old-map robo-movelist (parse-data dataset)]
            [warehouse-map (widen-map old-map)]
            [robot-init-pos (robot-start warehouse-map)]
            [after-moves (robot-in-motion.2 robot-init-pos warehouse-map robo-movelist)])
       (hash-fold (lambda (p c prev)
                    (if (char=? c #\[)
                        (+ prev (* 100 (real-part p)) (* 1 (imag-part p)))
                        prev))
                  0 after-moves)))))
