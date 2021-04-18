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


; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

(define (found? x l)
  (local ((define f (find x l)))
    (if (equal? f #false) #false
        (equal? (first f) x))))

(check-expect (find 2 '(1 2 3)) '(2 3))

(check-expect (found? 2 '(1 2 3)) #true)
(check-expect (found? 5 '(1 2 3 4)) #false)



; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)

(define (n-inside-playground? len)
  (lambda (l)
    (local ((define (check-in-playground l0)
              (cond [(empty? l0) #true]
                    [(and (< (posn-x (first l0)) WIDTH)
                          (< (posn-y (first l0)) HEIGHT))
                     (check-in-playground (rest l0))]
                    [else #false])))
      (and (= len (length l))
           (check-in-playground l))))
    )
 
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))

; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(define (random-posns n)
  (build-list
    n
    (lambda (i)
      (make-posn (random WIDTH) (random HEIGHT)))))

