(use-modules (statprof)
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
  "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
")

(define *part-17-data*
  (call-with-input-file "./17.txt" get-string-all))

(define-peg-pattern dataset body
  (and reg-a reg-b reg-c nl program-list))
(define-peg-pattern reg-a all
  (and (ignore "Register A:") ws num nl))
(define-peg-pattern reg-b all
  (and (ignore "Register B:") ws num nl))
(define-peg-pattern reg-c all
  (and (ignore "Register C:") ws num nl))
(define-peg-pattern program-list all
  (and (ignore "Program:") ws (* (or num (ignore ","))) nl))
(define-peg-pattern num all
  (+ (range #\0 #\9)))
(define-peg-pattern nl none
  "\n")
(define-peg-pattern ws none
  (or "\t" " "))

(define-record-type <3-bit>
  (make-3-bit register-a register-b register-c inst-pointer program-listing o/p)
  3-bit?
  (register-a get-register-a set-register-a!)
  (register-b get-register-b set-register-b!)
  (register-c get-register-c set-register-c!)
  (inst-pointer get-ipointer set-ipointer!)
  (program-listing get-listing)
  (o/p get-o/p set-o/p!))

(define initialize-3-bit
  (match-lambda
    [(('reg-a ('num a)) ('reg-b ('num b)) ('reg-c ('num c)) ('program-list ('num ps) ..1))
     (make-3-bit (string->number a)
                 (string->number b)
                 (string->number c)
                 0
                 (map string->number ps)
                 '())]))

(define (parse-data data)
  (let ([parse-tree (peg:tree (match-pattern dataset data))])
    (initialize-3-bit parse-tree)))

(define (combo-operand o puter)
  (match o
    [(or 0 1 2 3) o]
    [4 (get-register-a puter)]
    [5 (get-register-b puter)]
    [6 (get-register-c puter)]
    [7 '()]))

(define (get-literal-operand puter)
  (let* ([ip (get-ipointer puter)]
         [pl (get-listing puter)])
    (list-ref pl (1+ ip))))

(define (advance-ipointer-and-return puter)
  (let ([ip (get-ipointer puter)])
    (set-ipointer! puter (+ 2 ip))
    puter))

(define (adv puter)
  (let* ([na (get-register-a puter)]
         [o (get-literal-operand puter)]
         [nd (combo-operand o puter)])
    (set-register-a! puter (truncate (/ na (expt 2 nd))))
    (advance-ipointer-and-return puter)))

(define (bxl puter)
  (let* ([nb (get-register-b puter)]
         [lo (get-literal-operand puter)])
    (set-register-b! puter (logxor nb lo))
    (advance-ipointer-and-return puter)))

(define (bst puter)
  (let* ([o (get-literal-operand puter)]
         [cn (modulo (combo-operand o puter) 8)])
    (set-register-b! puter cn)
    (advance-ipointer-and-return puter)))

(define (jnz puter)
  (let* ([na (get-register-a puter)]
         [o (get-literal-operand puter)])
    (if (zero? na)
        (advance-ipointer-and-return puter)
        (begin
          (set-ipointer! puter o)
          puter))))

(define (bxc puter)
  (let* ([nb (get-register-b puter)]
         [nc (get-register-c puter)])
    (set-register-b! puter (logxor nb nc))
    (advance-ipointer-and-return puter)))

(define (out puter)
  (let* ([o (get-literal-operand puter)]
         [op (modulo (combo-operand o puter) 8)])
    (set-o/p! puter (append (get-o/p puter) (list op)))
    (advance-ipointer-and-return puter)))

(define (bdv puter)
  (let* ([na (get-register-a puter)]
         [o (get-literal-operand puter)]
         [nd (combo-operand o puter)])
    (set-register-b! puter (truncate (/ na (expt 2 nd))))
    (advance-ipointer-and-return puter)))

(define (cdv puter)
  (let* ([na (get-register-a puter)]
         [o (get-literal-operand puter)]
         [nd (combo-operand o puter)])
    (set-register-c! puter (truncate (/ na (expt 2 nd))))
    (advance-ipointer-and-return puter)))

(define *opcode-alist*
  `((0 . ,adv)
    (1 . ,bxl)
    (2 . ,bst)
    (3 . ,jnz)
    (4 . ,bxc)
    (5 . ,out)
    (6 . ,bdv)
    (7 . ,cdv)))

(define (execute-instruction puter)
  (let* ([ip (get-ipointer puter)]
         [pl (get-listing puter)])
    ((assoc-ref *opcode-alist* (list-ref pl ip)) puter)))

(define (debugging 3-puter)
  (let* ([puter 3-puter]
         [length-pl (length (get-listing puter))]
         [ip (get-ipointer puter)])
    (while (< ip length-pl)
      (set! puter (execute-instruction puter))
      (set! ip (get-ipointer puter)))
    (get-o/p puter)))

(define (solve-17.1 dataset)
  (statprof
   (lambda ()
     (let* ([3-puter (parse-data dataset)])
       (string-join (map number->string (debugging 3-puter)) ",")))))

;; hardcoded for Day 17 input
(define (a->target-o/p? a target-o/p)
  (let ([3-puter (parse-data *part-17-data*)])
    (set-register-a! 3-puter a)
    (list= = target-o/p (debugging 3-puter))))

(define (find-a-for-target target-o/p)
  (match target-o/p
    [(_) (first-ec #f (:integers a)
                   (if (a->target-o/p? a target-o/p))
                   a)]
    [(_ . rest) (* 8 (find-a-for-target rest))]))

(define (solve-17.2)
  (statprof
   (lambda ()
     (let* ([3-puter (parse-data *part-17-data*)])
       (find-a-for-target (get-listing 3-puter))))))
