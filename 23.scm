(use-modules (statprof)
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
  "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
")

(define *part-23-data*
  (call-with-input-file "./23.txt" get-string-all))

(define-peg-pattern dataset body
  (+ (and lan-pair nl)))
(define-peg-pattern lan-pair body
  (and compid (ignore "-") compid))
(define-peg-pattern compid all
  (+ (range #\a #\z)))
(define-peg-pattern nl none
  "\n")

(define (parse-data data)
  (peg:tree (match-pattern dataset data)))

(define (find-lan-neighbors-and-trios comp-pairs)
  (let ([lan-table (make-hash-table)]
        [trios '()])
    (for-each (match-lambda
                [(('compid c1) ('compid c2))
                 (begin
                   (hash-set! lan-table c1 (cons c2 (hash-ref lan-table c1 '())))
                   (hash-set! lan-table c2 (cons c1 (hash-ref lan-table c2 '()))))])
              comp-pairs)
    (hash-for-each (lambda (c1 connections-1)
                     (do-ec (:list c2 connections-1)
                            (:let connections-2 (hash-ref lan-table c2))
                            (:list c3 connections-2)
                            (if (member c3 connections-1))
                            (:let trio (sort (list c1 c2 c3) string<))
                            (set! trios (lset-adjoin equal? trios trio))))
                   lan-table)
    (values lan-table trios)))

(define (lan-password lan-computers lan-table)
  (let ([max-clique '()])
    (let bron-kerbosch ([r '()] [p lan-computers] [x '()])
      (if (and (null? p) (null? x))
          (when (< (length max-clique) (length r))
            (set! max-clique r))
          (do-ec (:list v p)
                 (let ([neighbors (hash-ref lan-table v)])
                   (bron-kerbosch (lset-adjoin string=? r v)
                                  (lset-intersection string=? p neighbors)
                                  (lset-intersection string=? x neighbors))
                   (set! p (delete v p))
                   (set! x (lset-adjoin string=? x v))))))
    (string-join (sort max-clique string<) ",")))

(define (solve-23 data)
  (statprof
   (lambda ()
     (let* ([comp-pairs (parse-data data)]
            [lan-table trios (find-lan-neighbors-and-trios comp-pairs)]
            [lan-computers (hash-fold (lambda (k _ kl)
                                        (cons k kl))
                                      '() lan-table)])
       (values (sum-ec (:list trio trios)
                       (if (any (cut string-prefix? "t" <>) trio))
                       1)
               (lan-password lan-computers lan-table))))))
