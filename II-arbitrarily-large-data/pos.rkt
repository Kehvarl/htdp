;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

(define (pos? alon)
  (cond
    [(empty? alon) #true]
    [(< (first alon) 0) #false]
    [else (pos? (rest alon))]))