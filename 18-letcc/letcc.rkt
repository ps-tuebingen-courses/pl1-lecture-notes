#lang racket

(define (f n) (+ 10 (* 5 (let/cc k (/ 1 (if (zero? n) (k 1) n))))))

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

(define breakpoint false) ; initalized with a dummy value

; the repl variable stores the continuation that jumps to the read-eval-print loop
(define repl false)       ; initialized with a dummy value

(define (break) (let/cc k
                  (begin
                    (set! breakpoint k)
                    (repl))))

(define (continue)
  (breakpoint))

(define (main)
  (display "1")
  (break)
  (display "2")
  (break)
  (display "3"))

; nothing to do after this, hence k is the REPL continuation
(let/cc k
  (set! repl k))

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

