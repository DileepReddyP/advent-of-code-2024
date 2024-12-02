(use-modules (statprof)
             (ice-9 peg)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 receive)
             (ice-9 textual-ports))

(define example-data
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
")
(define part-2-data
  (get-string-all (open-file "./2.txt" "r")))

(define-peg-pattern dataset body
  (* dataline))
(define-peg-pattern dataline body
  (and (+ (or level ws)) (ignore "\n")))
(define-peg-pattern ws none
  (or "\t" " "))
(define-peg-pattern level all
  (+ (range #\0 #\9)))

(define (parse-data data)
  (peg:tree (match-pattern dataset data)))

(define (get-number report)
  (map (compose string->number cadr) report))

(define (get-lists data)
  (let* ([parsed-tree (parse-data data)]
         [levels (map get-number parsed-tree)])
    levels))

(define (in-descending a b)
  (let ([diff (- a b)])
    (and (> diff 0) (< diff 4))))

(define (in-ascending a b)
  (let ([diff (- b a)])
    (and (> diff 0) (< diff 4))))

(define (is-safe pred report)
  (cond
   [(null? report) #t]
   [(null? (cdr report)) #t]
   [(not (pred (car report) (cadr report))) #f]
   [else (is-safe pred (cdr report))]))

(define (solve-2.1 dataset)
  (apply + (map (lambda (report)
                  (if (or (is-safe in-ascending report)
                          (is-safe in-descending report))
                      1
                      0))
                (get-lists dataset))))

(define (combine report)
  (map (lambda (i)
         (append (list-head report i) (list-tail report (1+ i))))
       (iota (length report))))

(define (is-safe-with-dampener pred report)
  (let [(combinations (combine report))]
    (any (cut is-safe pred <>) combinations)))

(define (solve-2.2 dataset)
  (length (filter (lambda (report)
                    (or (is-safe-with-dampener in-ascending report)
                        (is-safe-with-dampener in-descending report)))
                  (get-lists dataset))))

(statprof (lambda ()
            (solve-2.1 part-2-data)))

(statprof (lambda ()
            (solve-2.2 part-2-data)))
