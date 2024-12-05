(use-modules (statprof)
             (ice-9 peg)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 receive)
             (ice-9 textual-ports))

(define example-data "3   4
4   3
2   5
1   3
3   9
3   3
")

(define-peg-pattern dataset body
  (* dataline))
(define-peg-pattern dataline body
  (and l1 (+ ws) l2 (ignore "\n")))
(define-peg-pattern ws none
  (or "\t" " "))
(define-peg-pattern l1 all
  (+ (range #\0 #\9)))
(define-peg-pattern l2 all
  (+ (range #\0 #\9)))

(define (parse-data data)
  (peg:tree (match-pattern dataset data)))

(define (get-half id)
  (lambda (pair)
    (string->number (car (assoc-ref pair id)))))

(define (get-lists data)
  (let* ([parsed-tree (parse-data data)]
         [l1 (map (get-half 'l1) parsed-tree)]
         [l2 (map (get-half 'l2) parsed-tree)])
    (values l1 l2)))


(define (get-diffs l1 l2)
  (let* ([s-l1 (sort l1 <)]
         [s-l2 (sort l2 <)])
    (apply + (map (compose abs -) s-l1 s-l2))))

(receive [l1 l2]
    (get-lists example-data)
  (get-diffs l1 l2))


(define part-1-data (call-with-input-file "./1.txt" get-string-all))

(statprof (lambda ()
            (receive [l1 l2]
                (get-lists part-1-data)
              (get-diffs l1 l2))))

(define (get-sim-score l1 l2)
  (apply + (map * l1 (map (lambda (a)
                            (count (cut = a <>) l2)) l1))))

(receive [l1 l2]
    (get-lists example-data)
  (get-sim-score l1 l2))

(statprof (lambda ()
            (receive [l1 l2]
                (get-lists part-1-data)
              (get-sim-score l1 l2))))
