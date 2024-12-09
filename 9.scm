(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define example-data
  "2333133121414131402
")

(define part-9-data
  (call-with-input-file "./9.txt" get-string-all))

(define (char->number c)
  (- (char->integer c) 48))

(define (parse-data-1 disk-map)
  (let* ([disk-map-length (string-length disk-map)]
         [free-space-positions '()]
         [occupied-positions '()]
         [current-index 0])
    (do-ec (:parallel (:range file-size-index 0 disk-map-length 2)
                      (:range free-space-index 1 disk-map-length 2))
           (:let file-id (/ file-size-index 2))
           (:let file-size (char->number (string-ref disk-map file-size-index)))
           (begin
             (do-ec (:range f file-size)
                    (set! occupied-positions
                          (acons (+ current-index f) file-id occupied-positions)))
             (set! current-index (+ current-index file-size))
             (let ([free-space (string-ref disk-map free-space-index)])
               (when (not (char=? free-space #\newline))
                 (set! free-space (char->number free-space))
                 (set! free-space-positions (append free-space-positions
                                                    (list-ec (:range f free-space)
                                                             (+ current-index f))))
                 (set! current-index (+ current-index free-space))))))
    (values free-space-positions occupied-positions)))


(define (checksum fsp op)
  (let ([lfsp (length fsp)])
    (sum-ec (:list o (index i) op)
            (:let np (if (< i lfsp) (list-ref fsp i) #f))
            (:let op (car o))
            (:let v (cdr o))
            (:let cs (if (and np (< np op)) (* v np) (* v op)))
            cs)))


(define (parse-data-2 disk-map)
  (let* ([disk-map-length (string-length disk-map)]
         [free-space-positions '()]
         [occupied-positions '()]
         [max-width 0]
         [current-index 0])
    (do-ec (:string c (index i) (substring disk-map 0 (1- disk-map-length)))
           (begin
             (if (even? i)
                 (let ([file-size (char->number c)]
                       [file-id (/ i 2)])
                   (when (> file-size 0)
                     (set! occupied-positions
                           (acons current-index (cons file-id file-size) occupied-positions))
                     (set! current-index (+ current-index file-size))))
                 (let ([free-space (char->number c)])
                   (when (> free-space 0)
                     (set! max-width (max max-width free-space))
                     (set! free-space-positions
                           (acons current-index free-space free-space-positions))
                     (set! current-index (+ current-index free-space)))))))
    (values (reverse! free-space-positions) occupied-positions max-width)))

(define (less-car a b)
  (< (car a) (car b)))

(define (re-sort-and-checksum fsp op mw)
  (let ([new-pos '()]
        [to-remove '()]
        [nfsp fsp])
    (do-ec (:list o-pair op)
           (:let pos (car o-pair))
           (:let file-id (cadr o-pair))
           (:let file-size (cddr o-pair))
           (:let possible-pos (and (<= file-size mw)
                                   (first-ec #f (:list fs nfsp)
                                             (:let p (car fs))
                                             (:let s (cdr fs))
                                             (if (>= s file-size))
                                             p)))
           (and possible-pos (< possible-pos pos))
           (:let free-space (assoc-ref nfsp possible-pos))
           (:let ds (- free-space file-size))
           (begin
             (set! to-remove (cons pos to-remove))
             (set! new-pos
                   (acons possible-pos (cons file-id file-size) new-pos))
             (when (positive? ds)
               (set! nfsp (sort (acons (+ possible-pos file-size) ds nfsp) less-car)))
             (set! nfsp (assoc-remove! nfsp possible-pos))))
    (do-ec (:list tr to-remove)
           (set! op (assoc-remove! op tr)))
    (sum-ec (:list o-pair (append op new-pos))
            (:let pos (car o-pair))
            (:let file-id (cadr o-pair))
            (:let file-size (cddr o-pair))
            (:range i pos (+ pos file-size))
            (* file-id i))))


(define (solve-9.1 dataset)
  (statprof
   (lambda ()
     (let ([fsp op (parse-data-1 dataset)])
       (checksum fsp op)))))

(define (solve-9.2 dataset)
  (statprof
   (lambda ()
     (let ([fsp op mw (parse-data-2 dataset)])
       (re-sort-and-checksum fsp op mw)))))
