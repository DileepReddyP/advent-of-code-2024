(use-modules (statprof)
             (srfi srfi-1)
             (srfi srfi-42)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 textual-ports))

(define part-11-data
  (call-with-input-file "./11.txt" get-string-all))

(define example-data
  "125 17
")

(define (parse-data dataset)
  (let ([stone-map (make-hash-table)])
    (do-ec (:list s (string-split (string-drop-right dataset 1) #\space))
           (hash-set! stone-map (string->number s) 1))
    stone-map))

(define (stone/2 stone)
  (let* ([sl (string-length stone)]
         [sl/2 (/ sl 2)])
    (values (string->number (substring stone 0 sl/2))
            (string->number (substring stone sl/2 sl)))))

(define (stone-map-set! stone-map stone c)
  (hash-set! stone-map stone (+ c (hash-ref stone-map stone 0))))

(define (split-stones stone-map)
  (let ([new-stone-map (make-hash-table)]
        [even-length? (compose even? string-length)])
    (hash-for-each
     (lambda (stone c)
       (cond
        [(zero? stone) (stone-map-set! new-stone-map 1 c)]
        [(number->string stone) even-length? => (lambda (s)
                                                 (let ([l r (stone/2 s)])
                                                    (stone-map-set! new-stone-map l c)
                                                    (stone-map-set! new-stone-map r c)))]
        [else (stone-map-set! new-stone-map (* stone 2024) c)]))
     stone-map)
    new-stone-map))

(define (stone-count stone-map)
  (hash-fold (lambda (_ c p) (+ c p)) 0 stone-map))

(define (split-stones-n dataset n)
  (let ([stone-map (parse-data dataset)])
    (do-ec (:range i n)
           (set! stone-map (split-stones stone-map)))
    (stone-count stone-map)))

(define (solve-11 dataset)
  (statprof
   (lambda ()
     (values (split-stones-n dataset 25)
             (split-stones-n dataset 75)))))

