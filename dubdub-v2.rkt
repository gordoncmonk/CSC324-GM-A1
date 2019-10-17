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
  (interpret (hash) prog))

#|
(interpret env expr) -> any
  env: hash?
    The environment with which to evaluate the expression.
  expr: datum?
    A syntactically-valid Dubdub expression.

  Returns the value of the Dubdub expression under the given environment.
|#
(define (interpret env expr)
  (cond
    [(null? expr) null]
    [(list? expr)
     (cond
       [(literal? (first expr))      (literal-helper expr env)]
       [(builtin? (first expr))      (builtin-helper expr env)]
       [(dub-proc? (first expr) env) (hash-ref env (first expr))]
       [else                         (expr-helper expr env)])] ;[GM] TODO: Need recursivity for entire expression
    [else (hash-ref env expr expr)])) #| Second expr in case of no value for expr key; just take the expr as is |#

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

(define (dub-proc? expr env) ;[GM] TODO: Update this!
  (hash-has-key? env expr))

; Returns whether a given symbol refers to a builtin Dubdub function.
(define (builtin? identifier) (hash-has-key? builtins identifier))

; Returns the builtin procedure corresponding to "identifier" to apply.
(define (builtin-helper expr env)
  (cond
    [(eq? (first expr) 'procedure?) (void)]
    [(= (length (rest expr)) 1) (apply (hash-ref builtins (first expr)) (interpret env (second expr)))]
    ;[(= (length (rest expr)) 1) (apply (hash-ref builtins (first expr)) (interpret env (second expr)))]
    ;[(= (length (rest expr)) 2) (apply (hash-ref builtins (first expr)) (list (interpret env (second expr)) (interpret env (third expr))))]
    [else (apply (hash-ref builtins (first expr)) (map (map-calc env) (rest expr))) ]
          ;(list (interpret env (second expr)) (interpret env (third expr))))
          ; Recursive Structure of function
          
; Returns whether a datum is a boolean or number literal.
(define (literal? datum) (or (boolean? datum) (number? datum)))

; Return a boolean or number literal; throw an error if calling literal as a procedure.
(define (literal-helper expr env)
  (if (> (length expr) 1)
      (report-error 'not-a-function (first expr))
      (first expr)))

#|
Starter definition for a closure "struct". Racket structs behave similarly to
C structs (contain fields but no methods or encapsulation).
Read more at https://docs.racket-lang.org/guide/define-struct.html.

You can and should modify this as necessary. If you're having trouble working with
Racket structs, feel free to switch this implementation to use a list/hash instead.
|#
(struct closure (params body))

#|
(expr-helper expr env) -> any
  env: hash?
    The environment with which to evaluate the expression.
  expr: datum?
    A syntactically-valid Dubdub expression.

  Returns the value of the Dubdub expression under the given environment.
|#
(define (expr-helper expr env)
  (define (expr-handler expr env)
    (cond
      [(list? (first expr))
       (if (member (first (first expr)) '(define define-contract))
           (interpret (interpret env (first expr)) (rest expr))
           (interpret (hash-set env (first expr) (interpret env (first expr))) expr))]
      [(dub-proc? (first expr) env) (apply (interpret env (first expr)) (rest expr))]
      [else (report-error 'unbound-name (first expr))]))
  (cond
    [(eq? (first expr) 'any)             (void)]
    [(eq? (first expr) 'define)          (define-helper expr env)]
    [(eq? (first expr) 'define-contract) (void)]
    [(eq? (first expr) 'lambda)          (list 'closure expr env)]
    [(eq? (first expr) '->)              (void)]
    [else                                (expr-handler expr env)]))
;[else                                (interpret (interpret env (first expr)) (rest expr))]))

#|
(define-helper expr env) -> any(?)
  expr: datum?
    ...
  env: hash?
    ...

  Returns...
|#
(define (define-helper expr env)
  (cond
    [(null? expr)                     null]
    [(= (length (rest expr)) 2)       (hash-setter (rest expr) env)]
    ;[(= (length (rest expr)) 0)       (void)]
    [else                             (report-error 'arity-mismatch (length (rest expr)) 2)]))

;Hash Setting
(define (hash-setter expr env)
  (cond
    [(null? expr)             (hash)]
    [(literal? (second expr)) (hash-set env (first expr) (second expr))]
    [(list? (second expr))    (hash-set env (first expr) (interpret env (second expr)))]
    [else                     (hash-set env (first expr) (hash-ref env (second expr) (second expr)))]))

;  (if (null? bindings)
;      null
;      (if (number? (second bindings))
;          (hash-set env (first bindings) (second bindings))
;          (if (list? (second bindings))
;              (hash-set env (first bindings) (interpret env (second bindings)))
;              (hash-set env (first bindings) (hash-ref env (second bindings) (second bindings)))))))
;(hash-set env (first bindings) (dict-ref env (second bindings)))))))

(define ((map-calc env) expr) ;Helper function that's a curry of eval-calc to allow me to map
  (interpret env expr))

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
    [else (append (list (list (first lst) (second lst))) (list-setup (rest (rest lst))))]))

#| Syntax |#
; [GM] From lec04.pdf, slide 24
(define-syntax list-comp
  (syntax-rules (: <-)
    [(list-comp <out-expr> : <id> <- <list-expr>)
     (map (lambda (<id>) <out-expr>)
          <list-expr>)]))

