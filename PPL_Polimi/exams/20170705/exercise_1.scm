#lang racket
; 1) Define a mutable binary tree data structure, using structs.

(struct leaf ((value #:mutable)))

(struct branch (
    (left #:mutable)
    (right #:mutable)
    ))

(define (tree-show t)
  (cond
    [(leaf? t) (begin
                 (display "(leaf ")
                 (display (leaf-value t))
                 (display ")"))]
    [(branch? t) (begin
                   (display "(branch ")
                   (tree-show (branch-left t))
                   (display " ")
                   (tree-show (branch-right t))
                   (display ")"))]
    [else (error "Not a tree.")]))

; 2) Define a destructive map operation for binary trees, called tmap!.
; E.g. (define t1 (branch (branch (leaf 1)(leaf 2))(leaf 3)))
; after (map! (lambda (x) (+ x 1)) t1), t1 becomes
; (branch (branch (leaf 2)(leaf 3))(leaf 4))

(define (tmap! f t)
  (cond
    [(leaf? t) (set-leaf-value! t (f (leaf-value t)))]
    [(branch? t) (begin
                   (tmap! f (branch-left t))
                   (tmap! f (branch-right t)))]
    [else (error "Not a tree")]))

; 3) Define a destructive reverse, called reverse!, which takes a binary
; tree and keeps it structure, but “reversing” all the values in the leaves.
; E.g. (reverse! t1) makes t1 the tree (branch (branch (leaf 3)(leaf 2))(leaf 1)).

(define (fringe t)
  (cond
    [(leaf? t) (list (leaf-value t))]
    [(branch? t) (append (fringe (branch-left t)) (fringe (branch-right t)))]
    [else (error "Not a tree")]))

(define (reverse! t)
  ; Helper function to set values from a list
  (define (helper t xs)
    (cond
      [(leaf? t) (begin
                   (set-leaf-value! t (car xs))
                   (cdr xs))]
      [(branch? t) (let* ((ys (helper (branch-left t) xs))
                          (ys1 (helper (branch-right t) ys)))
                     ys1)]
      [else (error "Not a tree.")]))
                   
  ; Get the fringe in reverse order
  (let ((rFringe (reverse (fringe t))))
    ; Reverse tree in place
    (helper t rFringe)))  