(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (aoc-2024-common)
             (ice-9 string-fun)
             (ice-9 textual-ports))

(define *example-data*
  "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
")

(define *part-19-data*
  (call-with-input-file "./19.txt" get-string-all))

(define-peg-pattern dataset body
  (+ (or (ignore ",") patterns (ignore " "))))
(define-peg-pattern patterns all
  (+ (or "w" "u" "b" "r" "g")))

(define-cached (find-pattern-match design patterns-list)
  (if (zero? (string-length design))
      1
      (sum-ec (:list p patterns-list)
              (if (string-prefix? p design))
              (find-pattern-match (substring design (string-length p)) patterns-list))))

(define (solve-19 data)
  (statprof
   (lambda ()
     (let* ([split-str (string-split (string-replace-substring data "\n\n" "*") #\*)]
            [pattern-str design-str (car+cdr split-str)]
            [design-list (remove string-null? (string-split (car design-str) #\newline))]
            [pattern-list (map cadr (peg:tree (match-pattern dataset data)))]
            [possible-designs (list-ec (:list d design-list)
                                       (:let matches (find-pattern-match d pattern-list))
                                       (not (zero? matches))
                                       matches)])
       (values (length possible-designs)
               (apply + possible-designs))))))
