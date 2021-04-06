#lang racket

; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum l)
  (fold1 + 0 l))
	
; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (fold1 * 1 l))

(define (fold1 f t l)
  (cond
    [(empty? l) t]
    [else
     (f (first l)
        (fold1 f t (rest l)))]))
