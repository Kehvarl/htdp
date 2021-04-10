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

;[]
(define-struct inventory [name description acquisition-price sales-price])

(define my-inventory (list
                       (make-inventory "Item 1" "The third thing" 100.00 150.00)
                       (make-inventory "Item 2" "Thing 1" 25.00 25.00)
                       (make-inventory "Item 3" "Secondus" 50.00 75.00)))

(define (sort-inventory list-of-items)
  (local (
          (define (price-difference item)
            (- (inventory-sales-price item) (inventory-acquisition-price item)))
          (define (compare-items item1 item2)
            (< (price-difference item1) (price-difference item2))))
    (sort list-of-items compare-items)))

              
