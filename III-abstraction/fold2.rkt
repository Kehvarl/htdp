#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(define (fold2 f bc l)
  (cond
    [(empty? l) bc]
    [else
     (f (first l)
        (fold2 bc (rest l)))]))

; [List-of Number] -> Number
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product
         (rest l)))]))


; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))
	
; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))
