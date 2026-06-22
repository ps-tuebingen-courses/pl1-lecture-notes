# Monadic Reflection

The content of this chapter is available as a Racket file [here.](./monadic-reflection.rkt)

```racket
#lang racket
(require racket/control)
```

We have already seen that monadic style can encode continuation-passing style:
the continuation monad *is* CPS, dressed up as a monad. It is natural to ask
whether the converse holds — whether continuations are powerful enough to
recover monadic programming. They are, in a precise and surprising sense due to
Andrzej Filinski, who introduced the notion of *monadic reflection*. The result
can be summarised as a slogan: *delimited continuations let us write
direct-style programs that nevertheless run in an arbitrary monad.*

It is worth being clear about why this is desirable. Direct style — writing
`(+ e1 e2)` rather than `(bind e1 (lambda (x) (bind e2 (lambda (y) (return (+ x y))))))`
— is how we like to read and write programs. But it appears to commit us to a
fixed notion of computation: ordinary, effect-free evaluation. Monadic style is
the opposite trade-off: by threading `bind` everywhere we gain the freedom to
reinterpret "computation" as nondeterminism, failure, state, and so on, but we
pay for that freedom with syntactic noise and a commitment to one particular
monad. Monadic reflection dissolves the trade-off. We write in direct style and
*choose* the monad in which that direct-style code is to be understood.

We exemplify reflection with the **list monad**, where the effect is
nondeterminism, but nothing below is specific to lists; abstracting over the
concrete monad is straightforward and we do it explicitly further on.

```racket
(define (return x) (list x))
(define (bind m f)
  (append-map f m))
```

(`append-map` is just `(apply append (map f m))` — it maps `f`, which returns a
list, over `m` and concatenates the results.)

## What we borrow from delimited control

Reflection is built on `shift` and `reset`, the delimited control operators from
our chapter on continuations. Two of their properties are doing the real work
here, and it is worth naming them.

First, the continuations are *delimited*. `(reset e)` marks a boundary; a
`(shift k ...)` inside it captures the continuation only up to that boundary and
binds it to `k`. Crucially, that captured `k` is an ordinary function that
*returns a value* — namely whatever the delimited context computes up to the
`reset`. This is exactly the shape `bind` demands of its second argument: a
function `A -> M B`. Undelimited `call/cc` would not do, because the
continuation it captures never returns.

Second, the continuation may be invoked *more than once* (it is *multi-shot*).
For the list monad this is essential: each separate invocation of `k` explores
one alternative, and we concatenate the results. A one-shot continuation would
permit failure and state but not genuine branching.

## Reflection and reification (Filinski)

The two operations are mutual inverses. `reflect` turns a monadic value into an
ordinary value (relative to an enclosing `reset`); `reify` runs a direct-style
computation and packages its monadic effects back up as a monadic value.

```racket
; List[A] -> A   (quasi-type, see below)
(define (reflect m)
  (shift k (bind m k)))

; A -> List[A]   (quasi-type)
; reify is a macro: it places its argument e textually inside the reset,
; which both delays e's evaluation and ensures any shift inside e is captured
; by THIS reset.
(define-syntax-rule (reify e)
  (reset (return e)))
```

The macro deserves a word. `reify` must not evaluate its argument before the
`reset` is in place — otherwise a `shift` raised by some `reflect` inside `e`
would escape past this delimiter, or find none at all. Writing `reify` as a
macro guarantees that `e` is elaborated *inside* the dynamic extent of the
`reset`. 

A note on the type annotations. The "types" `List[A] -> A` and `A -> List[A]`
are wishful. No ordinary type system assigns `reflect` the type `List[A] -> A`,
because a single value of type `A` can stand for a whole list of possibilities
only by virtue of the control context it inhabits. A faithful account requires a
typed discipline for `shift`/`reset` in which the *answer type* records the
ambient monad (Danvy and Filinski). For our purposes, read `List[A] -> A` as the
slogan it is: inside a `reify`, a list may masquerade as one of its elements.

We can now write a direct-style addition that is nonetheless nondeterministic:

```racket
(reify (+ (reflect (list 1 2)) (reflect (list 3 4))))
```

## What actually happens

The expression above is, after the macro expands, `(reset (return (+ (reflect '(1 2)) (reflect '(3 4)))))`.
Reading it as monadic code, it is precisely the `bind`-nest we were trying to
avoid writing by hand:

```racket
(bind (list 1 2) (lambda (x)
  (bind (list 3 4) (lambda (y)
    (return (+ x y))))))
```

To see why, evaluate left to right. The first `reflect` runs
`(shift k (bind '(1 2) k))`, where `k` is the captured continuation
"plug a value into the first hole, then compute `(return (+ <hole> (reflect '(3 4))))`."
So we get `(bind '(1 2) k)`, i.e. `(append (k 1) (k 2))`.

Computing `(k 1)` resumes the program with `1` in the first hole, leaving
`(return (+ 1 (reflect '(3 4))))`. The second `reflect` captures *its* continuation
`k2` — "wrap `(+ 1 <hole>)` in `return`" — and produces `(bind '(3 4) k2)`,
which is `(append (return (+ 1 3)) (return (+ 1 4)))`, i.e. `'(4 5)`. Symmetrically
`(k 2)` yields `'(5 6)`. Concatenating: `'(4 5 5 6)`.

```racket
> (reify (+ (reflect (list 1 2)) (reflect (list 3 4))))
'(4 5 5 6)
```

The result is every pairwise sum, and its *order* mirrors the evaluation order of
the program. The arithmetic was written in plain direct style; reflection
supplied the monadic plumbing.

## The reflection laws

Calling `reflect` and `reify` "mutual inverses" is more than a metaphor. The two
governing equations hold *because* `return` and `bind` satisfy the monad laws,
which is the content of the title "Representing Monads."

The first law says that reifying a single reflected value gives that value back:

```racket
(reify (reflect m))  =  m
```

Unfolding, `(reify (reflect m))` is `(reset (return (reflect m)))`. The `reflect`
captures the continuation `return`, so this reduces to `(bind m return)`, which
is `m` by the **right-identity** monad law.

The second law says that reflecting a reified computation re-inlines it: within
an enclosing `reset`,

```racket
(reflect (reify e))  =  e
```

This one rests on **associativity** of `bind`: pulling a sub-computation out into
its own `reify` and immediately reflecting it back is the same as having left it
in place. Together the two laws make precise the sense in which direct style and
monadic style are interchangeable views of the same program.

## Abstracting over the monad

Nothing above mentioned lists except `return` and `bind`. Packaging those two
operations lets the *same* `reflect` and `reify` serve any monad. (This subsection
is optional and can be deferred to a tutorial.)

```racket
(struct monad (return bind))

(define (reflect M m)
  (shift k ((monad-bind M) m k)))

(define-syntax-rule (reify M e)
  (reset ((monad-return M) e)))
```

The list monad becomes one instance among many:

```racket
(define list-monad
  (monad (lambda (x) (list x))
         (lambda (m f) (append-map f m))))

> (reify list-monad
    (+ (reflect list-monad (list 1 2))
       (reflect list-monad (list 3 4))))
'(4 5 5 6)
```

Swapping in the Maybe monad changes the effect from branching to
short-circuiting failure, with no change to `reflect`/`reify`:

```racket
(define maybe-monad
  (monad (lambda (x) (cons 'just x))
         (lambda (m f) (if (eq? m 'nothing) 'nothing (f (cdr m))))))

> (reify maybe-monad
    (+ (reflect maybe-monad (cons 'just 3))
       (reflect maybe-monad 'nothing)))
'nothing
```

Here a single reflected `'nothing` aborts the surrounding addition: the captured
continuation is never invoked, so the failure propagates. The state monad works
the same way (its continuation is invoked exactly once, threading the store), and
it is the example that makes the direct-style payoff most vivid — direct-style
imperative code with an explicitly chosen, first-class notion of state.

## Backtracking: the n-queens problem

Nondeterminism plus reflection gives backtracking search almost for free. We
introduce one combinator, `fail`, which reflects the empty list — a choice with
no successful continuations, hence a pruned branch:

```racket
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
```

Read `queens` column by column. The `foldl` ranges over the `n` columns; it
ignores the column index (`_`) and accumulates `y`, the rows chosen so far, most
recent first. For each column, `(reflect (inclusive-range 1 n))` nondeterministically
*chooses a row* `next`. If that row is `safe` against the existing queens we
extend the partial board with `(cons next y)`; otherwise `(fail)` prunes this
choice. After `n` columns the accumulator is a complete solution.

Two things are worth pointing out. First, the pruning is *eager*: the
safety check happens before the next column is ever considered, so unsafe partial
boards are abandoned immediately. This is ordinary depth-first backtracking — the
control stack *is* the search stack — but we never wrote a backtracking loop. The
list monad's `bind`, summoned by `reflect`, performs the enumeration and the
concatenation of successes. Second, `safe` measures diagonals by the column
distance `n`: two queens `n` columns apart clash on a diagonal exactly when their
rows differ by `n`, which is what `(= x (+ c n))` and `(= x (- c n))` test.

`(reify (queens 8))` returns the list of all 92 solutions. Because lists are
eager, this computes *every* solution before returning the first. If you want
solutions on demand — say, the first valid board — replace the list monad with
the stream monad: identical reflection code, but the search is then lazy and you
can take results one at a time.

## Where this leads

Monadic reflection is the direct ancestor of *algebraic effects and handlers*.
With hindsight, `reflect` is a restricted form of *performing an effect* and
`reify` a restricted form of *handling* it; Filinski's later "Monads in Action"
makes the connection explicit, and the modern handler calculi of Plotkin and
Pretnar generalise it to multiple, separately handled effects. The same idea is
now a language feature: OCaml 5's effect handlers, and languages such as Koka,
Eff,  Unison, and Effekt (developed here at U Tübingen!) all let you program in direct style against effects you define
yourself — exactly the freedom reflection first demonstrated, built on exactly
the delimited continuations we used here.

## References

- A. Filinski, *Representing Monads*, POPL 1994. [doi:10.1145/174675.178047](https://doi.org/10.1145/174675.178047)
- A. Filinski, *Monads in Action*, POPL 2010.
- O. Danvy and A. Filinski, *A Functional Abstraction of Typed Contexts* (and related work on the type discipline for `shift`/`reset`).
- G. Plotkin and M. Pretnar, *Handlers of Algebraic Effects*, ESOP 2009.
