(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define *example-data*
  "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
")

(define *part-14-data*
  (call-with-input-file "./14.txt" get-string-all))

(define-peg-pattern dataset body
  (* dataline))
(define-peg-pattern dataline body
  (and (ignore "p=") num (ignore ",") num ws (ignore "v=") num (ignore ",") num nl))
(define-peg-pattern ws none
  (or "\t" " "))
(define-peg-pattern num all
  (and (? "-") (+ (range #\0 #\9))))
(define-peg-pattern nl none
  "\n")

(define (parse-data data)
  (peg:tree (match-pattern dataset data)))

(define (pos-after-steps n)
  (lambda (s v e)
    (modulo (+ s (* n v)) e)))

(define (find-positions width height)
  (match-lambda
    [(('num spx) ('num spy) ('num svx) ('num svy))
     (match-let* ([(px py vx vy) (map string->number (list spx spy svx svy))]
                  [100-steps (pos-after-steps 100)]
                  [(qw qh) (list (quotient width 2) (quotient height 2))]
                  [(cpx cpy) (list (100-steps px vx width) (100-steps py vy height))])
       (cond
        [(and (< cpx qw) (< cpy qh)) 1]
        [(and (< cpx qw) (> cpy qh)) 2]
        [(and (> cpx qw) (> cpy qh)) 3]
        [(and (> cpx qw) (< cpy qh)) 4]
        [else 0]))]))

(define (solve-14.1 dataset)
  (statprof
   (lambda ()
     (let* ([robot-data (parse-data dataset)]
            [quadrants (map (find-positions 101 103) robot-data)]
            [n=? (lambda (n) (cut = n <>))])
       (* (count (n=? 1) quadrants)
          (count (n=? 2) quadrants)
          (count (n=? 3) quadrants)
          (count (n=? 4) quadrants))))))

(define (data->robot-list dataset)
  (map
   (match-lambda
     [(('num spx) ('num spy) ('num svx) ('num svy))
      (match-let* ([(px py vx vy) (map string->number (list spx spy svx svy))])
        `((,px . ,py) (,vx . ,vy)))])
   (parse-data dataset)))

(define (unique-after-iter? robot-position-list width height)
  (let* ([next-pos (pos-after-steps 1)]
         [positions (make-hash-table)]
         [new-position-list (map (match-lambda
                                   [((px . py) (vx . vy))
                                    (let* ([nx (next-pos px vx width)]
                                           [ny (next-pos py vy height)]
                                           [n `(,nx . ,ny)]
                                           [v `(,vx . ,vy)])
                                      (hash-set! positions n #t)
                                      (list n v))])
                                 robot-position-list)])
    (values (= (hash-count (const #t) positions) (length new-position-list))
            new-position-list)))

(define (solve-14.2 dataset)
  (statprof
   (lambda ()
     (let ([robot-position-list (data->robot-list dataset)])
       (do-ec (:range i 1 (* 103 101))
              (let ([unique? npl (unique-after-iter? robot-position-list 101 103)])
                (when (zero? (modulo i 1000))
                  (format #t "Iteration ~a...\n" i))
                (when unique?
                  (format #t "Iteration ~a!!!\n" i)
                  (do-ec (:range y 103)
                         (begin
                           (do-ec (:range x 101)
                                  (format #t "~a" (if (assoc-ref npl (cons x y)) #\* #\.)))
                           (newline))))
                (set! robot-position-list npl)))))))

