#lang racket
;; Hashim Abdulla
;; Due date: 5/4/2025

;; Define our Either type implementation
(struct success (value) #:transparent)
(struct failure (message) #:transparent)

;; Utility functions for working with Either
(define (from-success default either)
  (if (success? either)
      (success-value either)
      default))

(define (from-failure default either)
  (if (failure? either)
      (failure-message either)
      default))

;; Helper function to check if an item is in a list
(define in-list?
  (λ (x lst)
    (not (false? (member x lst)))))

;; Safe division that returns Either
(define (safe-div x y)  ; num num -> Either
  (if (= y 0)
      (failure "Division by zero")
      (success (/ x y))))

;; Check if a string is a valid identifier
;; Must begin with an alphabetic character
;; Followed by 0 or more letters, digits, hyphens (-) or underscores (_)
(define (valid-id? id)
  (and (string? id)
       (> (string-length id) 0)
       (char-alphabetic? (string-ref id 0))
       (for/and ([c (in-string id)])
         (or (char-alphabetic? c)
             (char-numeric? c)
             (char=? c #\_)
             (char=? c #\-)))))

;; Main evaluator function that takes an expression and state
(define (eval expr state)
  (cond
    ;; Numeric literal
    [(equal? (first expr) 'num) 
     (values (success (second expr)) state)]
    
    ;; Variable reference
    [(equal? (first expr) 'id)
     (let ([var-name (second expr)])
       (if (hash-has-key? state var-name)
           (let ([value (hash-ref state var-name)])
             (if (eq? value 'undefined)
                 (values (failure (format "Variable '~a' is undefined" var-name)) state)
                 (values (success value) state)))
           (values (failure (format "Variable '~a' not defined" var-name)) state)))]
    
    ;; Arithmetic operations
    [(in-list? (first expr) '(div add sub mult))
     (let-values ([(x-result state) (eval (second expr) state)])
       (if (failure? x-result)
           (values x-result state)  ; Pass failure up
           (let-values ([(y-result state) (eval (third expr) state)])
             (if (failure? y-result)
                 (values y-result state)  ; Pass failure up
                 (let ([x (success-value x-result)]
                       [y (success-value y-result)])
                   (case (first expr)
                     [(div) (values (safe-div x y) state)]
                     [(add) (values (success (+ x y)) state)]
                     [(sub) (values (success (- x y)) state)]
                     [(mult) (values (success (* x y)) state)]))))))]
    
    ;; Variable definition
    [(equal? (first expr) 'define)
     (let ([var-name (second expr)])
       (if (not (valid-id? (symbol->string var-name)))
           (values (failure (format "Invalid variable name: '~a'" var-name)) state)
           (if (hash-has-key? state var-name)
               (values (failure (format "Variable '~a' already defined" var-name)) state)
               (if (= (length expr) 2)
                   ;; Just declare with undefined value
                   (values (success 'undefined) (hash-set state var-name 'undefined))
                   ;; Define with initial value
                   (let-values ([(value-result state) (eval (third expr) state)])
                     (if (failure? value-result)
                         (values value-result state)  ; Pass failure up
                         (values (success (success-value value-result)) 
                                 (hash-set state var-name (success-value value-result)))))))))]
    
    ;; Variable assignment
    [(equal? (first expr) 'assign)
     (let ([var-name (second expr)])
       (if (not (valid-id? (symbol->string var-name)))
           (values (failure (format "Invalid variable name: '~a'" var-name)) state)
           (if (not (hash-has-key? state var-name))
               (values (failure (format "Cannot assign to undefined variable '~a'" var-name)) state)
               (let-values ([(value-result state) (eval (third expr) state)])
                 (if (failure? value-result)
                     (values value-result state)  ; Pass failure up
                     (values (success (success-value value-result))
                             (hash-set state var-name (success-value value-result))))))))]
    
    ;; Remove variable
    [(equal? (first expr) 'remove)
     (let ([var-name (second expr)])
       (if (not (hash-has-key? state var-name))
           (begin
             (printf "Error: remove ~a: variable not defined, ignoring~n" var-name)
             (values (success 'removed) state))
           (values (success 'removed) (hash-remove state var-name))))]
    
    ;; Unknown operation
    [else (values (failure (format "Unknown operation: ~a" (first expr))) state)]))

;; Function to print the current state
(define (print-state state)
  (printf "Current state:~n")
  (for ([(key value) (in-hash state)])
    (printf "  ~a = ~a~n" key value)))

;; Main REPL loop
(define (repl)
  (let loop ([state (hash)])
    (printf "> ")
    (flush-output) ; Ensure prompt appears immediately
    (let ([input (read)])
      (cond
        [(or (eq? input 'quit) (eq? input 'exit)) 
         (printf "Exiting...~n")
         (void)]
        [(eof-object? input)
         (printf "End of input, exiting...~n")
         (void)]
        [else
         (let-values ([(result new-state) (eval input state)])
           (if (success? result)
               (printf "Success: ~a~n" (success-value result))
               (printf "Failure: ~a~n" (failure-message result)))
           (print-state new-state)
           (loop new-state))]))))

;; Start the REPL
(repl)

;; Test cases (these would run in the REPL)
;; '(num 5)
;; '(add (num 5) (mult (num 2) (num 3)))
;; '(sub (num 20) (div (add (mult (num 4) (num 5)) (num 10))(num 6)))
;; '(div (num 5) (sub (num 5) (num 5)))
;; '(define a)
;; '(define b (num 10))
;; '(assign a (num 5))
;; '(add (id a) (id b))
;; '(define c (add (id a) (id b)))
;; '(assign a (add (id a) (num 1)))
;; '(remove b)
;; '(remove z)