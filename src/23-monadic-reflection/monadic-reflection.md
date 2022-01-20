# Monadic Reflection

```racket
#lang racket

(require racket/control)

; Monadic style can encode continuation-passing style:
; just use the continuation monad

; but the converse is also true in a certain sense that was
; discovered by A. Filinski, whoh invented the notion of
; "monadic reflection". Here, we exemplify monadic reflection
; in terms of the list monad, but it is straightforward
; to abstract over the concrete monad and make this work
; for every monad.

; The list monad operations
(define (return x) (list x))
(define (bind m f)
  (apply append (map f m)))

; Monadic reflection (Filinski)

; List[A] -> A (quasi-type)
(define (reflect m)
  (shift k (bind m k)))

; A -> List[A] (quasi-type)
; this is a macro that transforms (reify e) into (reify-thunk (thunk e))
; its sole purpose is to prevent the evaluation of e and wrap it into a thunk
(define-syntaxes (reify)
     (syntax-rules ()
       [(_ e)
        (reify-thunk (thunk e))]))

(define (reify-thunk t)
  (reset (return (t))))

; Now we can write direct-style programs (in this case: a direct-style + function), yet use monadic features.
(reify (+ (reflect (list 1 2)) (reflect (list 3 4))))

; more information in "Representing Monads" by A. Filinski:
;https://doi.org/10.1145/174675.178047



; Example: Backtracking using monadic reflection
; The n-queens problem
; If partial results are desired, lists should be
; replaced by streams

(define (fail) (reflect empty))

; Nat List[Nat] Nat -> Bool
(define (safe x l n)
  (or (empty? l)
      (let ((c (first l)))
        (and (not (= x c))
             (not (= x (+ c n)))
             (not (= x (- c n)))
             (safe x (rest l) (+ n 1))))))

(define (queens n)
  (foldl (lambda (_ y)
           (let ((next (reflect (inclusive-range 1 n))))
             (if (safe next y 1)
                 (cons next y)
                 (fail))))
         empty
         (inclusive-range 1 n)))

(reify (queens 8))

```