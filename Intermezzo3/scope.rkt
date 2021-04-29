#lang htdp/isl+
(require 2htdp/abstraction)

(define (p1 x y)
  (+ (* x y)
     (+ (* 2 x)
        (+ (* 2 y) 22))))
 
(define (p2 x)
  (+ (* 55 x) (+ x 11)))
 
(define (p3 x)
  (+ (p1 x 0)
     (+ (p1 x 1) (p2 x))))


; [Non-empty-list X] -> X
; retrieves the last item of ne-l 
(check-expect (last-item '(a b c)) 'c)
(check-error (last-item '()))
(define (last-item ne-l)
  (match ne-l
    [(cons lst '()) lst]
    [(cons fst rst) (last-item rst)]))
