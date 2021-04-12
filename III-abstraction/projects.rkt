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
