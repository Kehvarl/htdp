;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname contains-name) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-names is one of: 
; – '()
; – (cons String List-of-names)
; interpretation a list of invitees, by last name

; List-of-Names -> Boolean
; determines whether "Flatt" is in a-list-of-names
(define (contains-flatt? a-list-of-names)
  (contains? "Flatt" a-list-of-names))

; String List-of-Names -> Boolean
; determins whether name is in a-list-of-names
(define (contains? name a-list-of-names)
   (cond
    [(empty? a-list-of-names) #false]
    [(cons? a-list-of-names)
     (if (string=? (first a-list-of-names) name)
         #true
         (contains? name (rest a-list-of-names)))]))

(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Find" '()))
              #false)
(check-expect (contains-flatt? (cons "Flatt" '()))
              #true)
(check-expect
  (contains-flatt? (cons "X" (cons "Y"  (cons "Z" '()))))
  #false)
(check-expect
  (contains-flatt?
    (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)