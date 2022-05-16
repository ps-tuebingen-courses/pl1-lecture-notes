# LetCC

The content of this chapter is available as a Racket file [here.](./letcc.rkt)

```racket
#lang racket
```

Racket is a language with so-called _first-class continuations_. It can reify the
current continuation automatically and on the fly. As you may imagine, creating a
continuation involves copying the stack, but there are less and more efficient ways of
obtaining the same effect.

Adding continuations to a language makes it easy to create a better web programming protocol,
as we shall see. But first-class continuations are much more general and give programmers
immense power in numerous contexts.

In Racket (and the related programming language Scheme), a continuation is created with
``let/cc``. It can be used to give the current continuation a name: ``(let/cc k ... k ...)``

Let's write some programs using continuations (try this in the Racket read-eval-print loop).

``(let/cc k (k 3))`` What is the continuation k here?

``(+ 1 (let/cc k (k 3)))`` What is the continuation k here?

Using ``let/cc`` for exception handling: ``let/cc`` acts as the "try", invoking ``k`` as the "throw".

```racket
(define (f n) (+ 10 (* 5 (let/cc k (/ 1 (if (zero? n) (k 1) n))))))
```

Here we simulate a very simple kind of exception handling mechanism
with first-class continuations.
Try evaluating ``(h)`` in the read-eval-print-loop.

```racket
(define exceptionhandler (lambda (msg) (display "unhandled exception")))

(define (f n)
  (+ 5
     (if (zero? n) (exceptionhandler "division by zero")
         (/ 8 n))))

(define (g n)
  (+ (f n) (f n)))

(define (h)
  (let/cc k
    (begin
      (set! exceptionhandler (lambda (msg) (begin
                                             (displayln msg)
                                             (k))))
      (displayln (g 1))
      (displayln (g 0))
      (displayln (g 2)))))
```      

Here we encode a simple debugger with support for breakpoints.

The breakpoint variable stores the continuation at the current breakpoint

```racket
(define breakpoint false) ; initalized with a dummy value

; the repl variable stores the continuation that jumps to the read-eval-print loop
(define repl false)       ; initialized with a dummy value
```

The ``break`` function captures the current continuation, stores it, and jumps to the REPL:

```racket
(define (break) (let/cc k
                  (begin
                    (set! breakpoint k)
                    (repl))))
```

To continue, we jump to the previously stored continuation:

```racket
(define (continue)
  (breakpoint))
```

Here is a simple test program of our "debugger":

```racket
(define (main)
  (display "1")
  (break)
  (display "2")
  (break)
  (display "3"))

; nothing to do after this, hence k is the REPL continuation
(let/cc k
  (set! repl k))
```

Let's now consider a more sophisticated usage of let/cc, namely to program a simple form
of cooperative multi-threading, often called _co-routines_. A co-routine designates points
in the routine where a switch to another routine should occur - a so-called yield point.

With let/cc we can program co-routines within the language, without having any dedicated
built-in support for it:

```racket
(define queue empty)

(define (empty-queue?)
  (empty? queue))

(define (enqueue x)
  (set! queue (append queue (list x))))

(define (dequeue)
  (let ((x (first queue)))
    (set! queue (rest queue))
    x))

(define (fork)
  (let/cc k
    (begin
      (enqueue (lambda () (k 1))) ; enqueue thunk
      0)))

(define (join)
  (if (not (empty-queue?))
      ((dequeue))
      'alljoined))

(define (yield)
  (let/cc k
    (enqueue k)
    ((dequeue))))  ; invoke thunk

(define (fact n) (if (zero? n) 1 (* n (fact (- n 1)))))
(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(define (printfibs n)
  (if (zero? n)
      (begin (print "Fertig mit fibs") (newline))
      (begin
        (print (format "Fib(~A)=~A" n (fib n)))
        (newline)
        (yield)
        (printfibs (- n 1)))))

(define (printfacts n)
  (if (zero? n)
      (begin (print "Fertig mit facts") (newline))
      (begin
        (print (format "Fact(~A)=~A" n (fact n)))
        (newline)
        (yield)
        (printfacts (- n 1)))))


(define (test-forkjoin)
  (if (= (fork) 0)
      (printfibs 8)
      (printfacts 12))
  (join)
  (if (= (fork) 0)
    (printfibs 10)
    (printfacts 8))
  (join))

```
