#lang racket
; The fold operations are very general, and can be used to implement many higher order functions.
; 1. Define map as a fold (left or right, your choice).

(define (mymap f list)
  (define (-helper v vs)
    (cons (f v) vs))
  (foldr -helper '() list))

; 2. Define filter as a fold (left or right, your choice).

(define (myfilter f list)
  (define (-helper v vs)
    (if (f v)
        (cons v vs)
        vs))
  (foldr -helper '() list))