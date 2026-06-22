#lang racket
(require racket/control)

(define (return x) (list x))
(define (bind m f)
  (append-map f m))

; List[A] -> A   (quasi-type, see below)
(define (reflect m)
  (shift k (bind m k)))

; A -> List[A]   (quasi-type)
; reify is a macro: it places its argument e textually inside the reset,
; which both delays e's evaluation and ensures any shift inside e is captured
; by THIS reset.
(define-syntax-rule (reify e)
  (reset (return e)))

(reify (+ (reflect (list 1 2)) (reflect (list 3 4))))

(bind (list 1 2) (lambda (x)
  (bind (list 3 4) (lambda (y)
    (return (+ x y))))))

> (reify (+ (reflect (list 1 2)) (reflect (list 3 4))))
'(4 5 5 6)

(reify (reflect m))  =  m

(reflect (reify e))  =  e

(struct monad (return bind))

(define (reflect M m)
  (shift k ((monad-bind M) m k)))

(define-syntax-rule (reify M e)
  (reset ((monad-return M) e)))

(define list-monad
  (monad (lambda (x) (list x))
         (lambda (m f) (append-map f m))))

> (reify list-monad
    (+ (reflect list-monad (list 1 2))
       (reflect list-monad (list 3 4))))
'(4 5 5 6)

(define maybe-monad
  (monad (lambda (x) (cons 'just x))
         (lambda (m f) (if (eq? m 'nothing) 'nothing (f (cdr m))))))

> (reify maybe-monad
    (+ (reflect maybe-monad (cons 'just 3))
       (reflect maybe-monad 'nothing)))
'nothing

(define (fail) (reflect empty))

; Nat List[Nat] Nat -> Bool
; Is row x safe against the already-placed queens in l, the nearest of which
; sits n columns away?
(define (safe x l n)
  (or (empty? l)
      (let ((c (first l)))
        (and (not (= x c))            ; same row?
             (not (= x (+ c n)))      ; same descending diagonal?
             (not (= x (- c n)))      ; same ascending diagonal?
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

