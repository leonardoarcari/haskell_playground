#lang racket
; Define a <*> operator for lists, defined as in Haskell’s Applicative Functors.
; E.g.
;    (<*> (list (lambda (x) (+ 1 x))
;               (lambda (x) (* 2 x)))
;         '(1 2 3))
;
; is the list ‘(2 3 4 2 4 6).

(define (<*> fList vList)
  (flatten
   (map
    (lambda (f)
      (map f vList))
   fList)))