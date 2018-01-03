#lang racket
; Exercise 1
; Question no. 1
(define (maki lst)
 (call/cc (lambda (exit)
            (for-each
                (lambda (x)
                    (call/cc (lambda (yield)
                        (exit (cons x yield))
                        )))
            lst))))

(define (doit)
 (let ((x (maki '(a b c d))))
    (when (cons? x)
        (displayln (car x))
        ((cdr x)))))

; Question no. 2
; Write a macro, called curried, for defining curried functions
; that works in this example way:
;     (define f (curried (x y) (+ x y)))
; so that f is a function that can be used e.g. like this:
;     ((f 3) 4)
; returns 7.

(define-syntax curried
  (syntax-rules ()
    [(_ (var1 var2) body)
     (lambda (var1)
       (lambda (var2) body))]))

