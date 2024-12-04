(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-71)
             (ice-9 textual-ports))

(define example-data
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
")


(define part-4-data
  (get-string-all (open-file "./4.txt" "r")))

(define-peg-pattern dataset body
  (* dataline))
(define-peg-pattern dataline body
  (and (+ (or x m a s)) nl))
(define-peg-pattern x all
  (ignore "X"))
(define-peg-pattern m all
  (ignore "M"))
(define-peg-pattern a all
  (ignore "A"))
(define-peg-pattern s all
  (ignore "S"))
(define-peg-pattern nl none
  "\n")

(define (parse-data data)
  (list->array 2 (peg:tree (match-pattern dataset data))))

(define dir-list
  (let ([dir-array (make-array #f 3 3)])
    (array-index-map! dir-array (lambda (i j) `(,(1- i) ,(1- j))))
    (apply append (array->list dir-array))))

(define (check-for-letter arr i j l)
  (and (array-in-bounds? arr i j)
       (equal? (array-ref arr i j) l)))

(define (check-xmas arr i j)
  (length
   (filter identity
           (map
            (match-lambda
              [(x y) (and (check-for-letter arr i j 'x)
                          (check-for-letter arr (+ i x) (+ j y) 'm)
                          (check-for-letter arr (+ i (* 2 x)) (+ j (* 2 y)) 'a)
                          (check-for-letter arr (+ i (* 3 x)) (+ j (* 3 y)) 's))])
            dir-list))))

(define (solve-4.1 dataset)
  (let* ([data-array (parse-data dataset)]
         [solution-array (apply (cut make-array #f <> <>) (array-dimensions data-array))]
         [xmas-search (lambda (i j)
                        (match (array-ref data-array i j)
                          ['x (check-xmas data-array i j)]
                          [_ 0]))])
    (statprof
     (lambda ()
       (array-index-map! solution-array xmas-search)
       (apply +
              (apply append
                     (array->list solution-array)))))))

(define (check-x-mas arr i j)
  (if (and (check-for-letter arr i j 'a)
           (or (and (check-for-letter arr (1- i) (1- j) 'm)
                    (check-for-letter arr (1+ i) (1+ j) 's))
               (and (check-for-letter arr (1- i) (1- j) 's)
                    (check-for-letter arr (1+ i) (1+ j) 'm)))
           (or (and (check-for-letter arr (1- i) (1+ j) 'm)
                    (check-for-letter arr (1+ i) (1- j) 's))
               (and (check-for-letter arr (1- i) (1+ j) 's)
                    (check-for-letter arr (1+ i) (1- j) 'm))))
      1
      0))

(define (solve-4.2 dataset)
  (let* ([data-array (parse-data dataset)]
         [solution-array (apply (cut make-array #f <> <>) (array-dimensions data-array))]
         [xmas-search (lambda (i j)
                        (match (array-ref data-array i j)
                          ['a (check-x-mas data-array i j)]
                          [_ 0]))])
    (statprof
     (lambda ()
       (array-index-map! solution-array xmas-search)
       (apply +
              (apply append
                     (array->list solution-array)))))))
