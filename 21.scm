(use-modules (statprof)
             (ice-9 q)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 hash-table)
             (ice-9 textual-ports))

(define *example-data*
  "029A
980A
179A
456A
379A
")

(define *part-21-data*
  (call-with-input-file "./21.txt" get-string-all))

(define (parse-data data)
  (remove string-null? (string-split data #\newline)))

(define *numeric-keypad*
  (alist->hash-table '((0.0+0.0i . #\7)
                       (0.0+1.0i . #\8)
                       (0.0+2.0i . #\9)
                       (1.0+0.0i . #\4)
                       (1.0+1.0i . #\5)
                       (1.0+2.0i . #\6)
                       (2.0+0.0i . #\1)
                       (2.0+1.0i . #\2)
                       (2.0+2.0i . #\3)
                       (3.0+1.0i . #\0)
                       (3.0+2.0i . #\A))))

(define *reverse-numeric-keypad*
  (let ([rnk (make-hash-table)])
    (hash-for-each (lambda (k v)
                     (hash-set! rnk v k))
                   *numeric-keypad*)
    rnk))

(define *directional-keypad*
  (alist->hash-table '((0.0+1.0i . up)
                       (0.0+2.0i . A)
                       (1.0+0.0i . left)
                       (1.0+1.0i . down)
                       (1.0+2.0i . right))))

(define *reverse-directional-keypad*
  (let ([rdk (make-hash-table)])
    (hash-for-each (lambda (k v)
                     (hash-set! rdk v k))
                   *directional-keypad*)
    rdk))

(define *direction-words*
  '(down right up left))

(define *direction-alist*
  (map cons *direction-words* grid-directions))

(define-cached (keypad-bfs directional? start-pos end-key)
  (let ([visited (make-hash-table)]
        [queue (make-q)]
        [min-distance (inf)]
        [all-paths '()]
        [keypad-map (if directional? *directional-keypad* *numeric-keypad*)])
    (enq! queue `(,start-pos . ()))
    (let loop ([bq queue])
      (if (positive? (q-length bq))
          (let* ([current-pos current-moves (car+cdr (deq! bq))]
                 [current-key (hash-ref keypad-map current-pos)])
            (if (eqv? current-key end-key)
                (begin
                  (when (<= (length current-moves) min-distance)
                    (set! min-distance (length current-moves))
                    (set! all-paths (cons (reverse (cons 'A current-moves)) all-paths)))
                  (loop bq))
                (begin
                  (hash-set! visited current-pos #t)
                  (do-ec (:list t *direction-words*)
                         (:let d (assoc-ref *direction-alist* t))
                         (:let next-pos (+ current-pos d))
                         (:let next-char (hash-ref keypad-map next-pos))
                         (and next-char
                              (not (hash-ref visited next-pos)))
                         (enq! bq `(,next-pos . ,(cons t current-moves))))
                  (loop bq))))
          all-paths))))

(define-cached (dpad-moves keypad-movelist depth)
  (match depth
    [0 (length keypad-movelist)]
    [_ (let ([current-start (hash-ref *reverse-directional-keypad* 'A)])
         (sum-ec (:list m keypad-movelist)
                 (:let paths-to-m (keypad-bfs #t current-start m))
                 (:let possible-keypresses (map (cut dpad-moves <> (1- depth)) paths-to-m))
                 (begin
                   (set! current-start (hash-ref *reverse-directional-keypad* m)))
                 (apply min possible-keypresses)))]))

(define (complexity-calc keypad-string depth)
  (let ([current-start (hash-ref *reverse-numeric-keypad* #\A)]
        [numeric-part (string->number (substring keypad-string 0 3))])
    (sum-ec (:string k keypad-string)
            (:let paths-to-k (keypad-bfs #f current-start k))
            (:let possible-keypresses (map (cut dpad-moves <> depth) paths-to-k))
            (begin
              (set! current-start (hash-ref *reverse-numeric-keypad* k)))
            (* numeric-part (apply min possible-keypresses)))))

(define (solve-21 data)
  (statprof
   (lambda ()
     (let ([passcodes (parse-data data)])
       (values (sum-ec (:list p passcodes)
                       (complexity-calc p 2))
               (sum-ec (:list p passcodes)
                       (complexity-calc p 25)))))))
