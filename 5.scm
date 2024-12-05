(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-26)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define example-data
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
")

(define part-5-data
  (call-with-input-file "./5.txt" get-string-all))

(define-peg-pattern dataset body
  (and
   (+ (and order-rule nl))
   nl
   (+ (and update-list nl))))
(define-peg-pattern order-rule all
  (+ (or page (ignore "|"))))
(define-peg-pattern update-list all
  (+ (or page (ignore ","))))
(define-peg-pattern page all
  (+ (range #\0 #\9)))
(define-peg-pattern nl none
  "\n")

(define (parse-data data)
  (apply append (peg:tree (match-pattern dataset data))))

(define-record-type <page-order>
  (make-page-order page-no prev-pages next-pages)
  page-order?
  (page-no get-page-no)
  (prev-pages get-prev-pages set-prev-pages!)
  (next-pages get-next-pages set-next-pages!))

(define (update-page-order-table order-table prev-page next-page)
  (when (not (hash-ref order-table prev-page))
    (hash-set! order-table prev-page (make-page-order prev-page '() '())))
  (when (not (hash-ref order-table next-page))
    (hash-set! order-table next-page (make-page-order next-page '() '())))
  (let ([prev-page-record (hash-ref order-table prev-page)]
        [next-page-record (hash-ref order-table next-page)])
    (set-next-pages! prev-page-record (cons next-page (get-next-pages prev-page-record)))
    (set-prev-pages! next-page-record (cons prev-page (get-prev-pages next-page-record))))
  order-table)

(define (split-page-order-and-lists data)
  (let ([order-table (make-hash-table)]
        [update-lists '()])
    (for-each
     (match-lambda
       [('order-rule ('page p) ('page n))
        (set! order-table
              (update-page-order-table order-table (string->number p) (string->number n)))]
       [('update-list ('page pages) ..1)
        (set! update-lists (cons (map string->number pages) update-lists))])
     data)
    (values order-table update-lists)))

(define (sort-page-order order-table page-list)
  (reverse
   (sort
    page-list
    (lambda (page-2 page-1)
      (let* ([page-1-record (hash-ref order-table page-1)]
             [should-be-after (get-next-pages page-1-record)])
        (member page-2 should-be-after))))))

(define (solve-5 dataset)
  (statprof
   (lambda ()
     (let* ([parsed-data (parse-data dataset)]
            [order-table update-lists (split-page-order-and-lists parsed-data)]
            [ols not-ols (partition
                          (lambda (page-list)
                            (every
                             (lambda (page-index)
                               (let* ([page-no (list-ref page-list page-index)]
                                      [page-record (hash-ref order-table page-no)])
                                 (and (null? (lset-intersection
                                              =
                                              (list-head page-list page-index)
                                              (get-next-pages page-record)))
                                      (null? (lset-intersection
                                              =
                                              (list-tail page-list (1+ page-index))
                                              (get-prev-pages page-record))))))
                             (iota (length page-list))))
                          update-lists)])
       (values
        (apply + (map (lambda (l) (list-ref l (/ (1- (length l)) 2))) ols))
        (apply + (map (lambda (l) (list-ref l (/ (1- (length l)) 2)))
                      (map (cut sort-page-order order-table <>) not-ols))))))))
