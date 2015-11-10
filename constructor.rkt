#lang racket

; TODO: NEED TO ADD DOCUMENTATION AND TEST

(define-syntax class
  (syntax-rules (class def __init__ self :)
    [(class <Class>
      def __init__(self <arg> ...):
       (<var> = <expr>) ...)
    (define <Class>
      (lambda ([<arg> 0] ... )
        (let* ([<var> <expr>] ...)
          (lambda (msg)
            (cond [(equal? (string-append "self." msg) (id->string <var>))
                   (if (procedure? <var>)
                       (<var>)
                       <var>)] ...
                  [else "Unrecognized message!"])))
        ))]))

; Test using example from page
(class MyClass
  def __init__(self a b):
  (r = (f a))
  (self.x = (f a))
  (self.y = (list b 100 r)) ; how would this work if I wanted to put a list
  (self.z = "you are cool"))

(define (f r)
  (+ r 5))

(define test (MyClass))
; (test) ; what should this return
(test "x")
(test "y")
(test "z")
; These work, they return the right things,
; not sure what the default value should be
; because itll work differently if its a
; string or a value

#|
(define func
  (lambda (x) (+ x 7)))

(class Point
  def __init__(self a b):
  (x = (func a))
  (y = b))

(define Point2
  (lambda ([a 0] [b 0])
     (let ([x (func a)]
           [y (list b 1 2 3)])
      (lambda (msg)
        (cond [(equal? msg (id->string x)) x]
              [(equal? msg (id->string y)) y]
              [else "Unrecognized message!"]
              )))))

(define p (Point))
(p "x")
(p "y")

(define p1 (Point 3))
(p1 "x")
(p1 "y")

(define p2 (Point 3 4))
(p2 "x")
(p2 "y")

#|
(class Point
  def __init__(self a b):
  (self.x = a)
  (self.y = b))

(define p (Point 


|#
|#

; -----------------------------------------------------------------------------
; Class macro. This section is just for your reference.
; -----------------------------------------------------------------------------
(define-syntax class-old
  (syntax-rules ()
    [(class-old <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))