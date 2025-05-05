#lang racket

(require data/maybe)

(define (safe-div x y)  ; num num -> maybe num
  (if (= y 0)
      nothing
      (just (/ x y))))

(define in-list?   ; item list -> bool 
  (λ (x lst)
    (not (false? (member x lst)))))


(define (eval expr)
  (cond
    [(equal? (first expr) 'num) (just (second expr))]
    [(in-list? (first expr) '(div add sub mult))
     (let(
          [x (eval (second expr))]
          [y (eval (third  expr))])
       (if (or (nothing? x) (nothing? y))
           nothing
           (cond
             [(equal? (first expr) 'div) (safe-div (from-just 1 x) (from-just 1 y))]
             [(equal? (first expr) 'add) (just(+ (from-just 1 x) (from-just 1 y)))]
             [(equal? (first expr) 'sub) (just (- (from-just 1 x) (from-just 1 y)))]
             [else (just(* (from-just 1 x) (from-just 1 y)))])))]
     [else nothing]))  ; unknown operation;not an int, not add, sub, mult, or div
       ; so return nothing

(eval '(num 5))
(eval '(add (num 5) (mult (num 2) (num 3))))
(eval '(sub (num 20) (div (add (mult (num 4) (num 5)) (num 10))(num 6))))
(eval '(div (num 5) (sub (num 5) (num 5)))) ;nothing (div by 0)
