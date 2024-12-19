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
  (* dataline))
(define-peg-pattern dataline body
  (+ (or (ignore ",") patterns ws)))
(define-peg-pattern patterns all
  (+ (range #\a #\z)))
(define-peg-pattern ws none
  " ")

(define patterns-to-list
  (match-lambda
    [(('patterns p) ..1) p]))

(define-cached (find-pattern-match towel patterns-list)
  (if (zero? (string-length towel))
      1
      (sum-ec (:list p patterns-list)
              (if (string-prefix? p towel))
              (find-pattern-match (substring towel (string-length p)) patterns-list))))

(define (solve-19 data)
  (statprof
   (lambda ()
     (let* ([split-str (string-split (string-replace-substring data "\n\n" "*") #\*)]
            [pattern-str to-match-str (car+cdr split-str)]
            [to-match-list (remove string-null? (string-split (car to-match-str) #\newline))]
            [pattern-list (patterns-to-list (peg:tree (match-pattern dataset data)))]
            [towel-patterns (list-ec (:list tm to-match-list)
                                     (:let matches (find-pattern-match tm pattern-list))
                                     (not (zero? matches))
                                     matches)])
       (values (length towel-patterns)
               (apply + towel-patterns))))))
