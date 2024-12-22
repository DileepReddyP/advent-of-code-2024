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
  "1
2
3
2024
")

(define *part-22-data*
  (call-with-input-file "./22.txt" get-string-all))

(define (parse-data data)
  (map string->number (remove string-null? (string-split data #\newline))))

(define (mix n1 n2)
  (logxor n1 n2))

(define (prune n)
  (modulo n 16777216))

(define (calc-next-secret old-secret)
  (let* ([step-1 (prune (mix (* old-secret 64) old-secret))]
         [step-2 (prune (mix (floor/ step-1 32) step-1))]
         [step-3 (prune (mix (* step-2 2048) step-2))])
    step-3))

(define (2000th-secret seed-list)
  (sum-ec (:list s seed-list)
          (fold-ec s (:range i 2000)
                   i (lambda (_ s)
                       (calc-next-secret s)))))

(define (prices/deltas secret-no prev-price i price/delta-list)
  (match i
    [0 (reverse price/delta-list)]
    [_ (let* ([next-secret (calc-next-secret secret-no)]
              [next-price (modulo next-secret 10)])
         (prices/deltas next-secret next-price (1- i)
                                    (cons `(,next-price . ,(- next-price prev-price))
                                          price/delta-list)))]))

(define (4-delta-price-table price/delta-list 4-delta-alist)
  (match price/delta-list
    [((_ . d1) (_ . d2) (_ . d3) (p . d4) . _)
     (4-delta-price-table (cdr price/delta-list)
                          (cons `(,(list d1 d2 d3 d4) . ,p) 4-delta-alist))]
    [_ (alist->hash-table (reverse 4-delta-alist))]))

(define (price-signal seed-list)
  (let ([sliding-window-price (make-hash-table)])
    (do-ec (:list s seed-list)
           (:let 4dpt (4-delta-price-table (prices/deltas s (modulo s 10) 2000 '()) '()))
           (hash-for-each (lambda (window price)
                            (hash-set! sliding-window-price window
                                       (+ price (hash-ref sliding-window-price window 0))))
                          4dpt))
    (hash-fold (lambda (window price prev)
                 (if (> price (cdr prev))
                     `(,window . ,price)
                     prev))
               '(() . 0)
               sliding-window-price)))

(define (solve-22 data)
  (statprof
   (lambda ()
     (let ([seed-list (parse-data data)])
       (values (2000th-secret seed-list)
               (price-signal seed-list))))))


