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
          
