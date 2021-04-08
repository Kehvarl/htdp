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
