#lang racket
; The function (cos-min i j), given below, returns the integer
; in the range [i, j] with the smallest cosine.

(define (cos-min i j)
  (if (= i j)
      j
      (let ((k (cos-min (+ i 1) j)))
        (if (< (cos i) (cos k))
            i
            k))))

; Implement a tail-recursive version of cos-min.

(define (tail-cos-min i j)
  (define (-helper i j curr-arg-min)
    (if (= i j)
        curr-arg-min
        (-helper (+ i 1) j
                 (if (< (cos i) (cos curr-arg-min))
                     i
                     curr-arg-min))))
  (-helper i j i))
      