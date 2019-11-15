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


; For Define Functions and Define-contracts
(define (environment definitions hashL)
  (cond
    [(equal? (first definitions) 'define)
     (if (hash-has-key? hashL (second definitions))
         (report-error 'duplicate-name (second definitions))
         (hash-set hashL (second definitions) (interpret hashL (third definitions)))
         )
     ]
    [(equal? (first definitions) 'define-contract)
     ;(null)
     ;(hash-set hashL (second definitions))
     ; Incomplete
     (let ([contract (append (list (second definitions) 'contract))])
       (cond
         [(hash-has-key? hashL contract)
          (report-error 'invalid-contract contract)]
         [else (hash-set hashL contract
                         (append (reverse (list-tail (reverse (third definitions)) 2)) (list (first (reverse (third definitions)))))
                         )]
         #;[else (append (reverse (list-tail (reverse (third definitions)) 2)) (list (first (reverse (third definitions)))))]
         ;[else (first (reverse (third definitions)))]
         ))

     ;(list-set (second definitions) 1  )
     ]
    [else "Unknown Error"]
#;(
 (lambda (x) (< 0 x))
 (lambda (x) (< 0 x))
 ->
 (lambda (x) (< 0 x))
 )
    ; Filler for now
    )
  )


  

(define (run-interpreter prog)
  ;(void)

  (if (> (length prog) 1)
      ;(interpret (foldl interpret (hash) (first prog))   (second prog) )
      (interpret (foldl environment (hash) (reverse (rest (reverse prog)))) (first (reverse prog)))
      (interpret (hash) (first prog))
      )
  )

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
  
  (cond
    [(null? expr) null]
    [(or (boolean? expr) (number? expr)) expr]
    
    [(list? expr)
     (cond
       [(equal? (first expr) 'lambda)
        (list 'closure expr env)
        ]
       [(builtin? (first expr))      (builtin-helper expr env)]
       [(and (not (hash-has-key? env (first expr))) (not (list? (first expr))))
        (report-error 'not-a-function expr)]
       ;Function Call\/
       ; not else, need to fix
       [else
        (cond
          [(equal? 'closure (first (interpret env (first expr))))
           (let ([bounds (map (interpret-map env) (rest expr))])
             (let ([body (interpret env (first expr))])
               ;merge second body with bounds
               ;and put into env
               ;evaluate third body
               (let ([contract (append (list (first expr) 'contract))])
                 ;(if ((append (list (first expr) 'contract))))
                 (if (hash-has-key? env contract)

                     ;(hash-ref env contract)
                     #;(apply (hash-ref env contract) (map (interpret-map env) (rest expr))
            )
                     ;(second expr) 1
                     (apply (first (hash-ref env contract)) (second expr))

                     ;No contract => I can use original code up to this point
                     (cond
                       [(= (length (second (second body))) (length bounds))
                        (interpret
                         (foldl hash-setter (third body) (list-setup (list-merger (second (second body)) bounds)) )
                         (third (second body)))]
                  
                       [(< (length (second (second body))) (length bounds))
                        (report-error 'arity-mismatch (length bounds) (length (second (second body))))]

                       ; second body > bounds
                       ; Curry
                       [else
                   
                        ;new expr
                        #;(list-set body 1 (list-set (second body) 1 (list-tail (second (second body) ) (length bounds))))

                        ;new env
                        #;(foldl hash-setter (third body) (list-setup (list-merger (reverse (list-tail (reverse (second (second body))) (- (length (second (second body))) (length bounds)))) bounds)) )
                   
                        (list-set
                         (list-set body 1 (list-set (second body) 1 (list-tail (second (second body) ) (length bounds))))
                         2
                         (foldl hash-setter (third body) (list-setup (list-merger (reverse (list-tail (reverse (second (second body))) (- (length (second (second body))) (length bounds)))) bounds)) )
                         )

                        ]
                       )
                 ))
               ))
           ]
          [else "error"]
                     
        ;expr = '((lambda (x) (+ x 1)) 1)
        ;(first (interpret env (first expr))) 'closure
        ;(map (interpret-map env) (rest expr)) ;(1)
        ;(second (interpret env (first expr)))
        )]
       )]
    ;[else (hash-ref env expr)]
    ;[else (hash-ref env expr [report-error 'unbound-name expr])]
    [else (if (hash-has-key? env expr)
              (hash-ref env expr)
              (report-error 'unbound-name expr))]
              ;expr)]
    ; '#hash()
    
    ; Need to raise error if no hash-ref
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


(define (builtin-helper expr env)
  (cond
    [(eq? (first expr) 'procedure?) (void)]
    [(= (length (rest expr)) 1)
     (apply (hash-ref builtins (first expr)) (interpret env (second expr)))]
    [(> (length (rest expr)) 1)
     (apply (hash-ref builtins (first expr)) (map (interpret-map env) (rest expr))
            )]))

;Map for Interpret
(define ( (interpret-map env) expr)
  ;Most likely want to curry env and then call map with this curry
  ;map is called on rest expr
  (interpret env expr)
  )

; Return a hash-table for environment
;(define (environment hashL definitions)

;Hash Setting
(define (hash-setter bindings env)
  (if (null? bindings)
      null
      (if (number? (second bindings))
          (hash-set env (first bindings) (second bindings))
          (hash-set env (first bindings) (interpret env (second bindings) ))
          )
      )
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



#|
Starter definition for a closure "struct". Racket structs behave similarly to
C structs (contain fields but no methods or encapsulation).
Read more at https://docs.racket-lang.org/guide/define-struct.html.

You can and should modify this as necessary. If you're having trouble working with
Racket structs, feel free to switch this implementation to use a list/hash instead.
|#
(struct closure (params body))
