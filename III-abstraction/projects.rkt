#lang htdp/isl+
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define-struct letter-counts [letter count])

(define (starts-with# letter dictionary)
  (cond
    [(empty? dictionary) 0]
    [(string=? letter (first (explode (first dictionary))))
     (add1 (starts-with# letter (rest dictionary)))]
    [else (starts-with# letter (rest dictionary))]))

; Dictionary -> [letter-counts]
; returns a list of how often each letter is first in the dictionary
(define (letter-frequency dict)
  (local (
          (define (starts-with-letter# letter lst)
            (cons (make-letter-counts letter (starts-with# letter dict))
                  lst)))
    (foldr starts-with-letter# '() LETTERS)))

;(define LETTER-FREQUENCY (letter-frequency AS-LIST))
(define LETTER-FREQUENCY (list (make-letter-counts "a" 1)))

; Dictionary Letter -> Letter-Count
; retuns the letter that is the most common fist-letter in the dictionary
(define (most-frequent dict)
  (local (
          (define (max-letter lc1 lc2)
            (cond [(< (letter-counts-count lc1) (letter-counts-count lc2))
                   lc2]
                  [else lc1])))
    (foldr max-letter (make-letter-counts "a" 0) LETTER-FREQUENCY)))

; Dictionary Letter -> Dictionary
; returns a new dictionary made up only of words starting with a given letter

(define (words-starting-with letter dict)
  (filter (lambda (w) (string=? letter (first (explode w))))
          dict))
    
; Dictionary -> List-of-Dictionary
(define (words-by-first-letter dict)
  (local (
          (define (add-dict letter)
            (words-starting-with letter dict)))
    (map add-dict LETTERS)))

(define (convert-euro list-of-usd)
  (map (lambda (usd) (/ usd 1.06))
       list-of-usd))

(define (convert FC list-of-f)
  (map (lambda (f) (* (/ 5 9) (- f 32)))
       list-of-f))

(define (translate list-of-posn)
  (map (lambda (p) (list (posn-x p) (posn-y p)))
       list-of-posn))

(define (list-to-exclusive n)
  (build-list n (lambda (x) x)))

(define (list-to-inclusive n)
  (build-list n (lambda (x) (+ x 1))))

(define (list-inf-series n)
  (build-list n (lambda (x) (/ 1 x))))

(define (list-evens n)
  (build-list n (lambda (x) (* 2 x))))

(define (diagonalize n i)
  (build-list n (lambda (j) (if (= i j) 1 0))))

(define (diagonal-box n)
  (build-list n (lambda (i) (diagonalize n i))))

(define (list-diagonals n)
  (build-list n (lambda (x) (diagonal-box (add1 x)))))
