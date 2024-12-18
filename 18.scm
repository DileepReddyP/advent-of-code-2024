(use-modules (statprof)
             (ice-9 q)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define *example-data*
  "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
")

(define *part-18-data*
  (call-with-input-file "./18.txt" get-string-all))

(define-peg-pattern dataset body
  (* dataline))
(define-peg-pattern dataline body
  (and num (ignore ",") num nl))
(define-peg-pattern num all
  (+ (range #\0 #\9)))
(define-peg-pattern nl none
  "\n")

(define (parse-data data)
  (peg:tree (match-pattern dataset data)))

(define* (initialize-byte-list data #:optional l)
  (let* ([parsed-data (parse-data data)]
         [bytes-needed (if l (list-head parsed-data l) parsed-data)])
    (map (match-lambda
           [(('num y) ('num x))
            (make-rectangular (exact->inexact (string->number x))
                              (exact->inexact (string->number y)))])
         bytes-needed)))

(define (create-byte-map byte-positions extent)
  (let ([byte-map (make-hash-table)])
    (do-ec (:list p byte-positions)
           (hash-set! byte-map p #\#))
    (do-ec (:range x (1+ extent))
           (:range y (1+ extent))
           (:let p (make-rectangular (exact->inexact x) (exact->inexact y)))
           (not (hash-ref byte-map p))
           (hash-set! byte-map p #\.))
    byte-map))

(define (byte-bfs byte-map start-pos end-pos)
  (let ([visited (make-hash-table)]
        [queue (make-q)])
    (hash-set! visited start-pos #t)
    (set! queue (enq! queue `(,start-pos . 0)))
    (let loop ([bq queue])
      (if (positive? (q-length bq))
          (let* ([current-pos current-cost (car+cdr (deq! bq))])
            (if (= current-pos end-pos)
                current-cost
                (begin
                  (do-ec (:list d grid-directions)
                         (:let next-pos (+ current-pos d))
                         (:let next-char (hash-ref byte-map next-pos))
                         (and next-char
                              (not (hash-ref visited next-pos))
                              (not (char=? next-char #\#)))
                         (begin
                           (hash-set! visited next-pos #t)
                           (set! bq (enq! bq `(,next-pos . ,(1+ current-cost))))))
                  (loop bq))))
          #f))))

(define (solve-18.1 dataset l extent)
  (statprof
   (lambda ()
     (let* ([byte-list (initialize-byte-list dataset l)]
            [byte-map (create-byte-map byte-list extent)])
       (byte-bfs byte-map
                 (make-rectangular (exact->inexact 0)
                                   (exact->inexact 0))
                 (make-rectangular (exact->inexact extent)
                                   (exact->inexact extent)))))))

(define (solve-18.2 dataset l extent)
  (statprof
   (lambda ()
     (let* ([init-byte-list (initialize-byte-list dataset l)]
            [byte-list (initialize-byte-list dataset)]
            [byte-map (create-byte-map init-byte-list extent)]
            [byte-length (length byte-list)]
            [byte-loc l]
            [byte-coord '()])
       (set! byte-loc
             (first-ec l (:range i l byte-length)
                       (:let falling-byte (list-ref byte-list (1- i)))
                       (begin
                         (hash-set! byte-map falling-byte #\#))
                       (not (byte-bfs byte-map
                                      (make-rectangular (exact->inexact 0)
                                                        (exact->inexact 0))
                                      (make-rectangular (exact->inexact extent)
                                                        (exact->inexact extent))))
                       (1- i)))
       (set! byte-coord (list-ref byte-list byte-loc))
       (format #f "~a,~a"
               (inexact->exact (imag-part byte-coord))
               (inexact->exact (real-part byte-coord)))))))
