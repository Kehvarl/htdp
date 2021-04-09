#lang htdp/isl

(define (convert-euro list-of-usd)
  (local (
      (define (usd->euro usd)
        (* 1.06 usd)))
    (map usd->euro list-of-usd)))


(define (convertFC list-of-f)
  (local (
          (define (f->c f)
            (* (- f 32) (/ 5 9))))
    (map f->c list-of-f)))

(define (translate list-of-posn)
  (local (
          (define (posn->pair p)
            (cons (posn-x p) (cons (posn-y p) '()))))
    (map posn->pair list-of-posn)))
