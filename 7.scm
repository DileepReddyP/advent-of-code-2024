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
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
")

(define part-7-data
  (call-with-input-file "./7.txt" get-string-all))

(define-peg-pattern dataset body
  (* dataline))
(define-peg-pattern dataline body
  (and num (ignore ":") (* (and ws num)) (ignore "\n")))
(define-peg-pattern ws none
  (or "\t" " "))
(define-peg-pattern num all
  (+ (range #\0 #\9)))

(define (parse-data data)
  (peg:tree (match-pattern dataset data)))

(define (2-func-permutations n)
  (match n
    [0 '(())]
    [n (apply append (map (lambda (l)
                            `(,(cons + l) ,(cons * l)))
                          (2-func-permutations (1- n))))]))

(define (|| a b)
  (string->number (string-append (number->string a) (number->string b))))

(define (3-func-permutations n)
  (match n
    [0 '(())]
    [n (apply append (map (lambda (l)
                            `(,(cons + l) ,(cons * l) ,(cons || l)))
                          (3-func-permutations (1- n))))]))

(define (apply-ops nums ops)
  (fold (lambda (ops n acc)
          (ops acc n))
        (car nums) ops (cdr nums)))

(define (find-valid-equations three?)
  (let ([permuter (if three? 3-func-permutations 2-func-permutations)])
    (lambda (calibration)
      (match calibration
        [(('num s-r) (('num s-vals) ..1))
         (let* ([res (string->number s-r)]
                [vals (map string->number s-vals)]
                [ops (permuter (1- (length vals)))]
                [possible-res (map (cut apply-ops vals <>) ops)])
           (if (any (cut = res <>) possible-res)
               res
               0))]))))

(define (solve-7 dataset)
  (statprof
   (lambda ()
     (let* ([data-array (parse-data dataset)])
       (values (apply + (map (find-valid-equations #f) data-array))
               (apply + (map (find-valid-equations #t) data-array)))))))
