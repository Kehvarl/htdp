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

(check-expect [(sorted string<?) '("b" "c")] #true)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #true)
 
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)
