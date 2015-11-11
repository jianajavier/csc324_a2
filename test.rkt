#lang racket
; Import choice API
(require "choice.rkt")

(define (insert lst val)
  (if (empty? lst)
      (list val)
      (-< (cons val lst)
          (cons (first lst)
                (insert (rest lst) val)))))

(define (permutation lst)
  (if (empty? lst)
      '()
      (insert (permutation (rest lst)) (first lst))))