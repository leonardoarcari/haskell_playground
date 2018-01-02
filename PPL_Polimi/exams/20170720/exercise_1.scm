#lang racket
(define (print+sub x y)
 (display x)
 (display " ")
 (display y)
 (display " -> ")
 (- x y))

(define (puzzle)
 (call/cc (lambda (exit)
 (define (local e)
 (call/cc
 (lambda (local-exit)
 (exit (print+sub e
 (call/cc
 (lambda (new-exit)
 (set! exit new-exit)
 (local-exit #f))))))))
 (local 6)
 (exit 2))))