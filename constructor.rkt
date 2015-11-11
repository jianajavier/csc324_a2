#lang racket

(define-syntax class
  (syntax-rules (class def __init__ self :) ; Added keywords
    [(class <Class>
       def __init__(self <arg> ...): ; Added section to define initialization function with any number of arguments
       (<var> = <expr>) ...) ; Can define any number of variables
    (define <Class> ; Attributes do not need to be defined upon instantiation
      (lambda ([<arg> 0] ... ) ; Takes any number of optional arguments (attributes)
        (let* ([<var> <expr>] ...) ; Turns body of the init function into local variables (which could be referenced within each other, hence the let*)
          (lambda (msg)
            (cond [(equal? (string-append "self." msg) (id->string <var>)) ; Makes sure to only return value for attributes defined with "self." and not only local to constructor
                   (if (procedure? <var>)
                       (<var>)
                       <var>)] ...
                  [else "Unrecognized message!"])))
        ))]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))

#|
; -----------------------------------------------------------------------------
; Constructor Definition.
; -----------------------------------------------------------------------------

 (class <ClassName>                    ; Define class name with keyword "class"
   def __init__(self <param> ...):     ; Define any number of parameters with keyword "self" as first parameter

   ; This section is used to define methods or attributes
   ; For a method/attribute to be accessible outside the constructor,
   ; It must be preceded with keyword "self."
   ; Syntax: (<var> = <expr>) OR (<var> = <var>) OR (<var> = <literal>)

   (r = (f a))                         ; This method is local to the constructor (cannot be referenced outside)
   (self.x = (f a))                    ; This line can also be written as (self.x = r)
   (self.y = (list b 100 r))
   (self.z = "you are cool")

   )                                   ; End of constructor definition

; -----------------------------------------------------------------------------
; Constructor Use.
; -----------------------------------------------------------------------------

> (class MyClass
    def __init__(self a b):
    (r = (f a))
    (self.x = (f a))
    (self.y = (list b 100 r))
    (self.z = "you are cool"))
> (define (f r)
    (+ r 5))

; EXAMPLE 1 (NO PARAMETERS)

> (define test (MyClass))               ; No parameters means it will use default value defined as 0 in macro
> (test "x")
5
> (test "y")
'(0 100 5)
> (test "z")
"you are cool"
> (test "r")
"Unrecognized message!"                 ; r is not accessible outside of the constructor

; EXAMPLE 2 (WITH PARAMETERS)

> (define test2 (MyClass 5 10))          ; Parameters means it will use default values 5 and 10
> (test2 "x")
10
> (test2 "y")
'(10 100 10)
> (test2 "z")
"you are cool"
> (test2 "r")
"Unrecognized message!"                 ; r is not accessible outside of the constructor

|#