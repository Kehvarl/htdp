#lang racket

; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (tabulate sin n))
	
; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (tabulate sqrt n))

; Number -> [List-of Number]
; tabulates tan between n 
; and 0 (incl.) in a list
(define (tab-tan n)
  (tabulate tan n))

; Number -> [List-of Number]
; tabulates squares between n 
; and 0 (incl.) in a list
(define (tab-sqr n)
  (tabulate sqr n))

(define (tabulate f n)
  (cond
    [(= n 0)(list (f 0))]
    [else
     (cons
      (f n)
      (tabulate f (sub1 n)))]))
