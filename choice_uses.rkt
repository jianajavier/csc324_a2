#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 4
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#

(define (add-to-all lst item)
  (if (empty? lst)
      empty
      (cons (add-to-all-helper (first lst) item)
            (add-to-all (rest lst) item))))

(define (add-to-all-helper lst item)
  (cons item lst))

(define (subsets-old lst)
  (if (empty? lst)
      (list empty)
      (remove-duplicates
       (append (subsets-old (rest lst))
               (add-to-all (append (subset-help (rest lst)) (list (rest lst)))
                           (first lst))))))

(define (subset-help lst)
  (if (empty? lst)
      (list empty)
      (cons (list (first lst)) (subset-help (rest lst)))))

(define (subsets lst)
  (rec-helper (subsets-old lst)))

(define (rec-helper lst)
  (if (equal? (length lst) 1)
      (-< (first lst))
  (-< (first lst) (rec-helper (rest lst)))))

; QUESTION 5
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.

|#
(define (sudoku-4 lst)
  (?- constraints
      (?- (stronger-combo lst)
          (list
           (?- (combo lst) (list
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)))
           (?- (combo lst) (list
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)))
           (?- (combo lst) (list
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)))
           (?- (combo lst) (list
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)
                            (-< 1 2 3 4)))))))

(define (constraints lst)
  (if (not (list? lst))
      #f
      (let* ([firstrow (first lst)]
             [secondrow (second lst)]
             [thirdrow (third lst)]
             [fourthrow (fourth lst)]
             [firstcolumn (list (first firstrow) (first secondrow) (first thirdrow) (first fourthrow))]
             [secondcolumn (list (second firstrow) (second secondrow) (second thirdrow) (second fourthrow))]
             [thirdcolumn (list (third firstrow) (third secondrow) (third thirdrow) (third fourthrow))]
             [fourthcolumn (list (fourth firstrow) (fourth secondrow) (fourth thirdrow) (fourth fourthrow))]
             [firstquarter (list (first firstrow) (second firstrow) (first secondrow) (second secondrow))]
             [secondquarter (list (third firstrow) (fourth firstrow) (third secondrow) (fourth secondrow))]
             [thirdquarter (list (first thirdrow) (second thirdrow) (first fourthrow) (second fourthrow))]
             [fourthquarter (list (third thirdrow) (fourth thirdrow) (third fourthrow) (fourth fourthrow))]
             )
        (and (no-duplicates firstrow)
             (no-duplicates secondrow)
             (no-duplicates thirdrow)
             (no-duplicates fourthrow)
             (no-duplicates firstcolumn)
             (no-duplicates secondcolumn)
             (no-duplicates thirdcolumn)
             (no-duplicates fourthcolumn)
             (no-duplicates firstquarter)
             (no-duplicates secondquarter)
             (no-duplicates thirdquarter)
             (no-duplicates fourthquarter)))))

(define (stronger-combo question)
  (lambda (lst)
    (and (no-duplicates lst)
         (find-same-structure-list question lst))))

(define (combo question)
  (lambda (lst)
    (and (no-duplicates lst)
         (find-same-structure
          (rec-helper question)
          lst))))

(define (find-same-structure-list solve lst2)
        (and (find-same-structure (first solve) (first lst2))
             (find-same-structure (second solve) (second lst2))
             (find-same-structure (third solve) (third lst2))
             (find-same-structure (fourth solve) (fourth lst2))))

(define (no-duplicates lst)
  (equal? (length (set->list (list->set lst))) (length lst)))

(define (find-same-structure lst1 lst2)
  (let ([value-count (find-values lst1)])
    (equal? (structure-help lst1 lst2) value-count)))

(define (structure-help lst1 lst2)
  (if (not (list? lst2))
      0
      (if (empty? lst1)
          0
          (if (equal? (first lst1) "")
              (structure-help (rest lst1) (rest lst2))
              (if (equal? (first lst1) (first lst2))
                  (+ 1 (structure-help (rest lst1) (rest lst2)))
                  (structure-help (rest lst1) (rest lst2)))))))
  

(define (find-values lst)
  (if (empty? lst)
      0
      (if (not (equal? (first lst) ""))
          (+ 1 (find-values (rest lst)))
          (find-values (rest lst)))))

; QUESTION 6
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>

 > (fold-< max 0 (sin (* (-< 1 2 3 4) (+ (-< 100 200) (-< 1 2)))))
 0.9948267913584064

|#
(define-syntax fold-<
  (syntax-rules ()
    [(fold-< combine init expr)
     (foldl combine init (all expr))]))