#lang racket
; 1) Define the iterate function, with two parameters f and v, that returns
; the infinite list
; (v f(v) f^2(v) ... f^n(v) ...).
; Hint: use delay and force, as seen in class.

(define (iterate f v)
  (define (helper f v n)
    (if (= n 0)
        (list v (delay (helper f v (+ n 1))))
        (list (f v) (delay (helper f (f v) (+ n 1))))))
  (delay (helper f v 0)))
    
; 2) Define take, like in Haskell, to get items out of an infinite list.
; E.g. (take 10 (iterate (lambda (x) (+ x 1)) 0))
; should return (0 1 2 3 4 5 6 7 8 9)

(define (take n ilist)
  (if (= n 0)
      '()
      (let ((x (force ilist)))
        (cons (first x) (take (- n 1) (second x))))))