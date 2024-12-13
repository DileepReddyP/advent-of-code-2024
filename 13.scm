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
  "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
")

(define part-13-data
  (call-with-input-file "./13.txt" get-string-all))

(define-peg-pattern dataset body
  (* dataline))
(define-peg-pattern dataline body
  (and button-a button-b prize (? nl)))
(define-peg-pattern button-a all
  (and (ignore "Button A:") ws (ignore "X+") x (ignore ",") ws (ignore "Y+") y nl))
(define-peg-pattern button-b all
  (and (ignore "Button B:") ws (ignore "X+") x (ignore ",") ws (ignore "Y+") y nl))
(define-peg-pattern prize all
  (and (ignore "Prize:") ws (ignore "X=") x (ignore ",") ws (ignore "Y=") y nl))
(define-peg-pattern x all
  num)
(define-peg-pattern y all
  num)
(define-peg-pattern ws none
  (or "\t" " "))
(define-peg-pattern num body
  (+ (range #\0 #\9)))
(define-peg-pattern nl none
  "\n")

(define (minimize-tokens part2?)
  (match-lambda
    [(('button-a ('x axs) ('y ays)) ('button-b ('x bxs) ('y bys)) ('prize ('x pxs) ('y pys)))
     (match-let* ([(ax ay bx by vx vy) (map string->number (list axs ays bxs bys pxs pys))]
                  [px (if part2? (+ 10000000000000 vx) vx)]
                  [py (if part2? (+ 10000000000000 vy) vy)]
                  [det (- (* ax by) (* ay bx))])
       (if (zero? det)
           0
           (let ([a (/ (- (* by px) (* bx py)) det)]
                 [b (/ (- (* ax py) (* ay px)) det)])
             (if (and (integer? a) (integer? b))
                 (+ (* 3 a) (* 1 b))
                 0))))]))

(define (parse-data data)
  (peg:tree (match-pattern dataset data)))

(define (solve-13 dataset)
  (statprof
   (lambda ()
     (let ([claw-machine-data (parse-data dataset)])
       (values (apply + (map (minimize-tokens #f) claw-machine-data))
               (apply + (map (minimize-tokens #t) claw-machine-data)))))))

