#lang racket
; A simply-linked circular list (called Clist from now on) is a list
; in which the last node points to the first node (see figure).
; It is sometimes useful to have a sentinel last node, i.e. a node that does not contain
; data. The sentinel is used e.g. to check if we have traversed the whole list.
; An empty list contains only the sentinel node, that points to itself.
; Define a data structure for Clists (hint: use struct), together with a
; constructor for an empty Clist, and a variant of the cons operation for Clists,
; which adds a new element as the head of the previous Clist.

(struct Clist
  ((data #:mutable)
   (next #:mutable)))

(define (make-clist)
  (let ((x (Clist 'end 'nil)))
    (set-Clist-next! x x)
    x))

(define (get-clist-end head)
  (if (equal? (Clist-data head) 'end)
      head
      (get-clist-end (Clist-next head))))

(define (clist-cons v node)
  (let ((x (Clist v node))
        (lend (get-clist-end node)))
    (set-Clist-next! lend x)
    x))

(define (clist-show head)
  (define (helper l)
    (let ((data (Clist-data l)))
      (unless (equal? data 'end)
        (display data)
        (display ", ")
        (helper (Clist-next l)))))
  (display "[")
  (helper head)
  (display "'end]")
  (newline))

; Define cmap, a map operation for Clists.

(define (cmap f head)
  (let ((data (Clist-data head)))
    (unless (equal? data 'end)
      (set-Clist-data! head (f data))
      (cmap f (Clist-next head)))))