#lang racket #| * CSC324 Fall 2019: Assignment 1 * |#
#|
Module: dubdub
Description: Assignment 1: A More Featureful Interpreter
Copyright: (c)University of Toronto, University of Toronto Mississauga 
               CSC324 Principles of Programming Languages, Fall 2019

The assignment handout can be found at

    https://www.cs.toronto.edu/~lczhang/324/files/a1.pdf

Please see the assignment guidelines at 

    https://www.cs.toronto.edu/~lczhang/324/homework.html
|#

(provide run-interpreter)

(require "dubdub_errors.rkt")


;-----------------------------------------------------------------------------------------
; Main functions (skeleton provided in starter code)
;-----------------------------------------------------------------------------------------
#|
(run-interpreter prog) -> any
  prog: datum?
    A syntactically-valid Dubdub program.

  Evaluates the Dubdub program and returns its value, or raises an error if the program is
  not semantically valid.
|#
(define (run-interpreter prog)
  (void))

#|
(interpret env expr) -> any
  env: hash?
    The environment with which to evaluate the expression.
  expr: datum?
    A syntactically-valid Dubdub expression.

  Returns the value of the Dubdub expression under the given environment.
|#
(define (interpret env expr)
  ;(void)
  (cond [(null? expr) "Null Error"]

        [(or (boolean? expr) (number? expr)) expr]
        ;[(number? expr) expr]
        ;[(boolean? expr) expr]
        ; Noticed the above two have the same output, so I just merged
        ; them into one
        [(list? expr)
         (cond [(equal? (first expr) 'lambda)
                
                (list 'closure expr env)]
                
                 ;Function Expression/definition

               ;[(procedure? (first expr)) (apply (first expr))] ;Function Call
               
               ;Same code I used for ex4 for evaluate
               ;(list? (first expr)) Pre-Ex4 Code
                ;(interpret 
                ; (hash-setter (list-setup (list-merger (second (first expr)) (rest expr))))
                ; (third (first expr)) ;(hash-setter (second expr) ))
                ; )

               ;Might need to change else statement if error checking later
               [else

                (let* ([closure (hash-ref env (interpret env (first expr)) (interpret env (first expr)))])
  
                  (interpret
                   ; Set-up Environment in the same way I did for Ex4
                   (foldl hash-setter (third closure) (list-setup (list-merger (second (second closure)) (map (map-calc env) (rest expr)))) )
                   (third (second closure))
                             )
                  )
                
                ]
               
               ;[else "Inner Unknown Error"]
               
               )]
       
        
        [else (hash-ref env expr expr)]
        ; Second expr here is in case there is no value for expr key
        ; Then I just take the expr as is
        )
  )


;-----------------------------------------------------------------------------------------
; Helpers: Builtins and closures
;-----------------------------------------------------------------------------------------
; A hash mapping symbols for Dubdub builtin functions to their corresponding Racket value.
(define builtins
  (hash
   '+ +
   'equal? equal?
   '< <
   'integer? integer?
   'boolean? boolean?
   ; Note: You'll almost certainly need to replace procedure? here to properly return #t
   ; when given your closure data structure at the end of Task 1!
   'procedure? procedure?
   ))

; Returns whether a given symbol refers to a builtin Dubdub function.
(define (builtin? identifier) (hash-has-key? builtins identifier))

#|
Starter definition for a closure "struct". Racket structs behave similarly to
C structs (contain fields but no methods or encapsulation).
Read more at https://docs.racket-lang.org/guide/define-struct.html.

You can and should modify this as necessary. If you're having trouble working with
Racket structs, feel free to switch this implementation to use a list/hash instead.
|#
(struct closure (params body))

;Hash Setting
(define (hash-setter bindings env)
  (if (null? bindings)
      null
      (if (number? (second bindings))
          (hash-set env (first bindings) (second bindings))
          (if (list? (second bindings))
              (hash-set env (first bindings) (eval-calc (second bindings) env))
              (hash-set env (first bindings) (hash-ref env (second bindings) (second bindings)))
              ;(hash-set env (first bindings) (dict-ref env (second bindings)))
              )
          )
      )
  )

(define ( (map-calc env) expr)               ;Helper function that's a curry of eval-calc to allow me to map
  (interpret env expr)
  )

;List Setup for Lambda

(define (list-merger lst1 lst2) ;Merges the two lists so I can use the same hash-function
  (cond
    [(null? lst1)     ; If the first list is empty
     #;(lst2)
     '()]           ; ... return the second list.
    [(null? lst2)     ; If the second list is empty
     #;(lst1)
     '()]           ; ... return the first list.
    [else        ; If both lists are non-empty
     (append (list (first lst1)) (append (list (first lst2)) (list-merger (rest lst1) (rest lst2)))) 
     ]
    )  ; ... make a recursively call, advancing over the first
  ; ... list, inverting the order used to pass the lists.
  )

(define (list-setup lst) ;Adds the list pairs together so I can use the same hash-function
  (cond
    [(null? lst) '()]
    [else (append (list (list (first lst) (second lst))) (list-setup (rest (rest lst))))]
    )
  )


