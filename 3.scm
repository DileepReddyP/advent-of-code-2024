(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-71)
             (ice-9 textual-ports))

(define example-data
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
")
(define part-3-data
  (call-with-input-file "./3.txt" get-string-all))

(define-peg-pattern dataset body
  (* dataline))
(define-peg-pattern dataline body
  (and (+ (or mulinstruct corrupted))))
(define-peg-pattern corrupted none
  (and (not-followed-by mulinstruct) peg-any))
(define-peg-pattern mulinstruct all
  (and (ignore "mul(") num (ignore ",") num (ignore ")")))
(define-peg-pattern num all
  (+ (range #\0 #\9)))

(define (parse-data data)
  (peg:tree (match-pattern dataset data)))

(define (mult-impl mi)
  (match mi
    [('mulinstruct ('num a) ('num b)) (* (string->number a) (string->number b))]))

(define (solve-3.1 dataset)
  (statprof (lambda ()
              (apply + (map mult-impl (parse-data dataset))))))

(define example-data-2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
")

(define-peg-pattern dataset-2 body
  (* dataline-2))
(define-peg-pattern dataline-2 body
  (and (+ (or mulinstruct doinstruct dontinstruct corrupted-2))))
(define-peg-pattern corrupted-2 none
  (and (not-followed-by (or mulinstruct doinstruct dontinstruct)) peg-any))
(define-peg-pattern doinstruct all
  (and (ignore "do()")))
(define-peg-pattern dontinstruct all
  (and (ignore "don't()")))

(define (parse-data-2 data)
  (peg:tree (match-pattern dataset-2 data)))

(define mul-active #t)

(define (mult-impl-2 mi)
  (match mi
    ['doinstruct (begin
                   (set! mul-active #t)
                   0)]
    ['dontinstruct (begin
                     (set! mul-active #f)
                     0)]
    [('mulinstruct ('num a) ('num b)) (if mul-active
                                          (* (string->number a) (string->number b))
                                          0)]))

(define (solve-3.2 dataset)
  (set! mul-active #t)
  (statprof (lambda ()
              (apply + (map mult-impl-2 (parse-data-2 dataset))))))
