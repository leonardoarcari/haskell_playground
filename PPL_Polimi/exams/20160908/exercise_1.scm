#lang racket
; In the academic literature, there is a concept of pictures defined as
; rectangular arrays of symbols, e.g.
;     abb
;     bab
; Such pictures can of course be memorized by row, as lists of list,
; e.g. the previous picture is ‘((a b b)(b a b)).
; Consider the language L of pictures where symbols are from the set
; {0,1}, and are square pictures with 1 on the diagonal and 0 elsewhere
; (e.g. ‘((1 0 0)(0 1 0)(0 0 1))).
; Define a procedure, called genFig, which takes a natural number
; n and returns the picture of L with side n

(define (oneAt p size)
  (define (helper p curr)
    (cond
      [(= p curr) (cons 1 (helper p (- curr 1)))]
      [(= curr 0) '()]
      [else (cons 0 (helper p (- curr 1)))]))
  (reverse (helper p size)))

(define (genFig n)
  (define (helper curr size)
      (if (= curr 0)
          '()
          (cons (oneAt curr size) (helper (- curr 1) size))))
  (reverse (helper n n)))
      