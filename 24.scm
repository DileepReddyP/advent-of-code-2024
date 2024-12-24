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
             (ice-9 string-fun)
             (ice-9 textual-ports))

(define *example-data*
  "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
")

(define *part-24-data*
  (call-with-input-file "./24.txt" get-string-all))


(define-peg-pattern wire-init body
  (+ (and wire-map (? nl))))
(define-peg-pattern wire-map body
  (and wire-name (ignore ":") ws (or "0" "1")))

(define-peg-pattern wire-logic body
  (+ (and wire-op nl)))
(define-peg-pattern wire-op body
  (and (or and-op or-op xor-op) ws (ignore "->") ws wire-name))
(define-peg-pattern and-op all
  (and wire-name ws (ignore "AND") ws wire-name))
(define-peg-pattern or-op all
  (and wire-name ws (ignore "OR") ws wire-name))
(define-peg-pattern xor-op all
  (and wire-name ws (ignore "XOR") ws wire-name))

(define-peg-pattern wire-name all
  (+ (or (range #\a #\z) (range #\0 #\9))))
(define-peg-pattern nl none
  "\n")
(define-peg-pattern ws none
  " ")

(define (make-wire-table wire-init)
  (let ([wire-table (make-hash-table)])
    (for-each (match-lambda
                [(('wire-name w) v) (hash-set! wire-table w (string->number v))])
              wire-init)
    wire-table))

(define (list->queue l)
  (let ([q (make-q)])
    (do-ec (:list c (reverse l))
           (enq! q c))
    q))

(define (populate-operations-list wire-ops)
  (map (match-lambda
         [(('and-op ('wire-name w1) ('wire-name w2)) ('wire-name wo))
          `(and ,w1 ,w2 ,wo)]
         [(('or-op ('wire-name w1) ('wire-name w2)) ('wire-name wo))
          `(or ,w1 ,w2 ,wo)]
         [(('xor-op ('wire-name w1) ('wire-name w2)) ('wire-name wo))
          `(xor ,w1 ,w2 ,wo)])
       (reverse wire-ops)))

(define (find-full-adder-errors wire-ops no-of-bits)
  (let ([final-carry (string-append "z" (number->string no-of-bits))])
    (remove (match-lambda
              [('and w1 w2 wo)
               (or (and (string=? w1 "x00") (string=? w2 "y00"))
                   (and (or (and (string-prefix? "y" w1)
                                 (string-prefix? "x" w2))
                            (and (string-prefix? "x" w1)
                                 (string-prefix? "y" w2)))
                        (not (string-prefix? "z" wo)))
                   (and (not (or (and (string-prefix? "y" w1)
                                      (string-prefix? "x" w2))
                                 (and (string-prefix? "x" w1)
                                      (string-prefix? "y" w2))))
                        (not (string-prefix? "z" wo))))]
              [('or w1 w2 wo)
               (and (not (or (and (string-prefix? "y" w1)
                                  (string-prefix? "x" w2))
                             (and (string-prefix? "x" w1)
                                  (string-prefix? "y" w2))))
                    (or (string=? wo final-carry)
                        (not (string-prefix? "z" wo))))]
              [('xor w1 w2 wo)
               (or (and (string=? w1 "x00") (string=? w2 "y00"))
                   (and (or (and (string-prefix? "y" w1)
                                 (string-prefix? "x" w2))
                            (and (string-prefix? "x" w1)
                                 (string-prefix? "y" w2)))
                        (not (string-prefix? "z" wo)))
                   (and (not (or (and (string-prefix? "y" w1)
                                      (string-prefix? "x" w2))
                                 (and (string-prefix? "x" w1)
                                      (string-prefix? "y" w2))))
                        (string-prefix? "z" wo)))])
            wire-ops)))

(define (parse-data data)
  (let* ([data-with-delim (string-replace-substring data "\n\n" "*")]
         [wires operations (car+cdr (string-split data-with-delim #\*))]
         [wire-init (peg:tree (match-pattern wire-init wires))]
         [wire-ops (peg:tree (match-pattern wire-logic (car operations)))]
         [wire-list (populate-operations-list wire-ops)])
    (values (make-wire-table wire-init)
            wire-list
            (/ (length wire-init) 2))))

(define (finish-wire-ops wire-table wire-q)
  (while (positive? (q-length wire-q))
    (match-let* ([(op w1 w2 wo) (deq! wire-q)]
                 [w1v (hash-ref wire-table w1)]
                 [w2v (hash-ref wire-table w2)])
      (if (and w1v w2v)
          (hash-set! wire-table wo (match op
                                     ['and (logand w1v w2v)]
                                     ['or (logior w1v w2v)]
                                     ['xor (logxor w1v w2v)]))
          (enq! wire-q (list op w1 w2 wo)))))
  wire-table)

(define (find-wire-value wire-table prefix)
  (let* ([wire-list (hash-fold (lambda (w _ zl)
                                 (if (string-prefix? prefix w)
                                     (cons w zl)
                                     zl))
                               '() wire-table)]
         [sorted-list (sort wire-list string<)]
         [wire-vals (map (cut hash-ref wire-table <>) sorted-list)])
    wire-vals))

(define (binary-list->decimal blist)
  (sum-ec (:list n (index p) blist)
          (* n (expt 2 p))))

(define decimal->binary-list
  (case-lambda
    [(decimal)
     (decimal->binary-list decimal '())]
    [(decimal blist)
     (match decimal
       [0 (reverse (cons 0 blist))]
       [1 (reverse (cons 1 blist))]
       [_ (decimal->binary-list (quotient decimal 2) (cons (remainder decimal 2) blist))])]))

(define (add-last-errors wire-list error-last)
  (let ([final-error (string-append "x" (number->string error-last))])
    (filter (match-lambda
              [(_ w1 w2 _) (or (string=? w1 final-error)
                               (string=? w2 final-error))])
            wire-list)))

(define (solve-24 data)
  (statprof
   (lambda ()
     (let* ([wire-table wire-list no-of-bits (parse-data data)]
            [x-vals (find-wire-value wire-table "x")]
            [y-vals (find-wire-value wire-table "y")]
            [x+y (+ (binary-list->decimal x-vals) (binary-list->decimal y-vals))]
            [wire-table-after (finish-wire-ops wire-table (list->queue wire-list))]
            [possible-errors (find-full-adder-errors wire-list no-of-bits)]
            [z-vals (find-wire-value wire-table-after "z")]
            [z (binary-list->decimal z-vals)]
            [xor-error-pos (length (take-while zero? (decimal->binary-list (logxor x+y z))))]
            [error-last-input (- no-of-bits xor-error-pos)]
            [last-errors (add-last-errors wire-list error-last-input)]
            [final-error-list (append possible-errors last-errors)])
       (values z
               (string-join (sort (map fourth final-error-list) string<) ","))))))
