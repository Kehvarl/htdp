#lang htdp/isl+

; [X X -> Boolean] -> [ [List-of X] -> Boolean ]
; produces a function that determines whether 
; some list is sorted according to cmp
(define (sorted cmp)
  (lambda (l)
    (local ((define (sorted/l l)
              (cond
                [(or (empty? l) (empty? (rest l))) #true]
                [(cmp (first l) (second l))
                 (sorted/l (rest l))]
                [else #false])))
      (sorted/l l)
      )))

(define (sorted? cmp l)
  ((sorted cmp) l))

; [List-of X] [X X -> Boolean] -> [[List-of X] -> Boolean]
; is l0 sorted according to cmp
; are all items in list k members of list l0
(define (sorted-variant-of k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains? k l0)
         (contains? l0 k))))

; [List-of X] [List-of X] -> Boolean 
; are all items in list k members of list l
(define (contains? l k)
  (andmap (lambda (in-k) (member? in-k l)) k))


(check-expect [(sorted string<?) '("b" "c")] #true)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #true)
 
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

(check-expect [(sorted-variant-of '(3 2) <) '(2 3)]
              #true)
(check-expect [(sorted-variant-of '(3 2) <) '(3)]
              #false)

(check-expect (contains? '(1 2 3) '(1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(1 3)) #true)
