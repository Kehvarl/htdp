#lang htdp/isl+

; Shape Posn -> Boolean
(define (inside? s p)
  (s p))

; Number Number -> Shape 
(define (mk-point x y) 
  (lambda (p)
    (and (= (posn-x p) x) (= (posn-y p) y))))

; Number Number Number -> Shape
(define (mk-circle x y r)
  (local ((define (distance-from-center p)
            (sqrt (+ (* (- x (posn-x p)) (- x (posn-x p)))
                         (* (- y (posn-y p)) (- y (posn-y p)))))
            ))
    (lambda (p)
      (<= (distance-from-center p) r))))

(check-expect
  (inside? (mk-circle 3 4 5) (make-posn 0 0)) #true)
(check-expect
  (inside? (mk-circle 3 4 5) (make-posn 0 9)) #false)
(check-expect
 (inside? (mk-circle 3 4 5) (make-posn -1 3)) #true)


; Number Number Number Number -> Shape
; represents a width by height rectangle whose 
; upper-left corner is located at (ul-x, ul-y)
 
(check-expect (inside? (mk-rect 0 0 10 3)
                       (make-posn 0 0))
              #true)
(check-expect (inside? (mk-rect 2 3 10 3)
                       (make-posn 4 5))
              #true)
 
(define (mk-rect ul-x ul-y width height)
  (lambda (p)
    (and (<= ul-x (posn-x p) (+ ul-x width))
         (<= ul-y (posn-y p) (+ ul-y height)))))
          

; Shape Shape -> Shape
; combines two shapes into one 
(define (mk-combination s1 s2)
  ; Posn -> Boolean 
  (lambda (p)
    (or (inside? s1 p) (inside? s2 p))))

(define circle1 (mk-circle 3 4 5))
(define rectangle1 (mk-rect 0 3 10 3))
(define union1 (mk-combination circle1 rectangle1))

(check-expect (inside? union1 (make-posn 0 0)) #true)
(check-expect (inside? union1 (make-posn 0 9)) #false)
(check-expect (inside? union1 (make-posn -1 3)) #true)
