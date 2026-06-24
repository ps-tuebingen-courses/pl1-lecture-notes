# Monadic Reflection and Effect Handlers

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
operations lets the *same* `reflect` and `reify` serve any monad. 

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

## Where this leads: algebraic effects and handlers

Monadic reflection already contains the seed of a larger idea. Recall what
`reflect` and `reify` gave us: a single, ambient notion of effect (whatever monad
we had fixed), an operation `reflect` that *invokes* it, and a delimiter `reify`
that *interprets* it by running the captured continuation against `bind`.
*Algebraic effects and handlers* take exactly this structure and make it plural
and named. A program may use many different effects at once, each declared as a
signature of operations; it invokes them in direct style; and a *handler* — a
generalised `reify` — gives each effect its meaning by deciding what to do with
the delimited continuation. Where Filinski had one nameless effect per `reset`,
an effect system tracks a whole *set* of effects in the type and lets independent
handlers discharge them one at a time.

This is worth a teaser because it removes the specific frictions we hit with
monad transformers in the [monads](../20-monads-intro/monads-intro.html) and
[modular interpreters](../22-modular-interpreters/modular-interpreters.html)
chapters.

### The friction we are trying to remove

Three concrete costs from those chapters:

- **A transformer per monad.** Beside each monad we had to build a second
  artefact — `OptionT`, `ReaderT`, `StateT`, `ContT` — re-implementing the monad
  parameterised over an inner monad. The monad and its transformer are different
  objects with different code.
- **Lifting and forwarding boilerplate.** To reach an inner monad's operations
  through the stack we wrote the ladder `lift`, `lift2`, `lift3`, `lift4`, plus a
  `…Forwarder` trait for each combination (`ReaderStateMonadForwarder`,
  `ReaderContinuationMonadForwarder`). With *n* effects this tends toward *n²*
  hand-written forwarding cases, and, as we observed there, the lifting
  "sometimes destroys modularity."
- **Order baked into the type — and not always lawful.** The interaction of two
  effects is fixed by their position in the stack, so changing it means
  re-plumbing types; and some stacks are not even lawful. `ListT[IO]` was our
  cautionary tale: collapsing the list into a single `IO[List[A]]` let
  re-association reorder the inner prints, silently breaking associativity.

### Reflection again, as an effect

We use [Effekt](https://effekt-lang.org), a research language developed by
Brachthäuser and colleagues here in Tübingen, because its surface syntax lets us
re-express *this very chapter* almost line for line. The whole Effekt code in this
section can be edited and tried online [here](https://effekt-lang.org/playground.html?playground=fVTLjtpAELz7K0p7yVh4Fy3Z5OCIlVBOkaIciHJCPjhmAMvPjIdgZPHvqR4%2FYB%2BKBfK4Z7qqurptvdvpxMLoXc67KkJ8Txu7%2BVbayA%2FBG4D5%2FBrE%2FbNEA%2Bg2Tmx%2Bhj1o%2FDnGTXpvz7X2PG%2Brd7JZ1LlWA8R8xMfSA9S2mvg2jwEWke9j9jL8McATw4QjuVDkFICiKuPthwa%2F03IbIOZC7yqjQ8R1TSkZbAUdJwfoXBdaVCZVmcRWl%2Fw7YZK5WQVYR1Opq8hHhyzECsvnPrSOcBl2uRTNBYrYErjjGkjiRuNHmitfnngxcRNdt75WZaPaAG3jy1amWv%2BBEnW5VSJAMd5lF58Jl8Ewo9PdWcg61KbahyA0M9c31r1SZM2ZhzfuOA9T8aDlPxe9NNoeTRniZOLaGWt0c8wtAU%2BpPUxkHVThFDi9XHZo5VFOF5r1kI6EBJQDtCcLeoAMy%2BGQtE4uzxtmTBcpB6xhcT%2BtScs9h%2BNXmdrBgZNJrTbvWyCZUr9aB70HPQAbd3VC9UYEbATFjfW4TCmm2Tu0v3EOZQLklTyP5Sjf%2F%2BLCrnE82x8QnF5cnSbZdZi70Sa%2BBU4ZkWQwBJwu4e18c5NBZ8Bdcqg4IneYzdA%2BNIfqJLutR6rRL2Es4rQUAxxwzWptXio3JaSfXi9cZALZhaVM4FOAT%2B73OfJusoRjSu1tFo9dSdJGvmZXDEXBm0Hj411EP9XiGlkwEpExRN8H5T4FL1rylnqiHDW8Tz14dUsf4IZ3HGJyq%2Blz9Ircu%2FwD&repl=bWFpbigp).

Take our running example,

```racket
(reify (+ (reflect (list 1 2)) (reflect (list 3 4))))   ; ==> '(4 5 5 6)
```

and rebuild it in Effekt. The reflection of a list becomes a single effect
operation:

```effekt
effect reflect(m: List[Int]): Int   // List[Int] -> Int, exactly the quasi-type
```

A computation *uses* it with `do`, and its type records that the effect is still
open. The addition is now written in plain direct style:

```effekt
def example(): Int / reflect =
  (do reflect([1, 2])) + (do reflect([3, 4]))
```

What remains is to *interpret* `reflect`, and this is where the translation
becomes instructive. In the Racket version the monad was baked into `reflect`
itself, `(define (reflect m) (shift k (bind m k)))`, because there was one ambient
monad. In Effekt the operation carries no meaning at all; the `bind m k` moves
into the *handler*. We give it the list monad's own `bind` — the same definition
from the start of this chapter —

```effekt
// the list monad's bind, as before: apply k to each element, concatenate
def bind[A, R](m: List[A]) { k: A => List[R] }: List[R] =
  m match {
    case Nil()       => []
    case Cons(x, xs) => k(x).append(bind(xs){k})
  }
```

and `reify` becomes a handler that wraps the pure result with `return` (a
singleton list) and interprets each `reflect` as `bind m resume`:

```effekt
def reify[R] { prog: () => R / reflect }: List[R] =
  try { [ prog() ] }                                  // return: wrap the result
  with reflect { (m) => bind(m) { x => resume(x) } }  // bind m k, with k = resume
```

```effekt
reify { example() }   // ==> [4, 5, 5, 6]
```

Set the two side by side. Filinski's `reflect` is `(shift k (bind m k))`; the
Effekt handler clause is `bind(m) { x => resume(x) }`, with `resume` playing the
role of the captured continuation `k`. `reify`'s `reset (return e)` is the `try`
block `[ prog() ]`. The result `[4, 5, 5, 6]` is the same list, in the same
order, as the Racket `(4 5 5 6)`. Even `fail` transfers directly: reflecting the
empty list, `do reflect([])`, yields `bind [] resume = []`, exactly our
`(define (fail) (reflect empty))`.

The one thing that genuinely changed is *where the meaning lives*. Because the
monad now resides in the handler rather than in `reflect`, swapping the handler
swaps the monad with no change to `example` — which is precisely the
"abstract over the concrete monad" remark from earlier in this chapter, now made
real. And because the effect has a *name* and is *tracked in the type*, a single
program can mention several such effects at once. That is what the rest of the
section is about.

### Composing two genuinely different effects

The `reflect` handler above *is* the list monad — one effect, one handler. What
monad transformers are *for* is composing effects that come from *different*
monads, and that is where their machinery — a transformer per monad, the lifting
ladder, a fixed stack order — becomes heavy. So let us add a second, independent
effect.

The order-sensitive companion to nondeterminism is the Writer monad: a
computation that accumulates output. As an effect it is one operation,

```effekt
effect emit(msg: String): Unit
```

handled by collecting the emitted messages alongside the result:

```effekt
def writer[R] { prog: () => R / emit }: (R, List[String]) =
  try { (prog(), []) }
  with emit { (msg) => val (r, log) = resume(()); (r, Cons(msg, log)) }
```

Now a program that uses *both* effects, in plain direct style:

```effekt
def pick(): Int / { reflect, emit } = {
  val x = do reflect([1, 2])
  do emit("chose " ++ x.show)
  x
}
```

Notice what did *not* happen. `reify` was written to handle `reflect` and knows
nothing about `emit`; `writer` handles `emit` and knows nothing about `reflect`.
Yet `pick` uses both, and we may hand it to either handler: the effect a handler
does not discharge simply passes through to be handled further out. There is no
`WriterT`, no `lift`, no forwarder — the leftover effect flows outward on its own.
(This pass-through is Effekt's *contextual effect polymorphism*; it is the direct
replacement for the `lift`/`lift2`/`lift3`/`lift4` ladder.)

Because each handler discharges its own effect, the only remaining decision is
which one sits inside the other — and that decision, not a retyped transformer
stack, fixes their interaction:

```effekt
reify { writer { pick() } }
// ==> [(1, ["chose 1"]), (2, ["chose 2"])]   :  List[(Int, List[String])]

writer { reify { pick() } }
// ==> ([1, 2], ["chose 1", "chose 2"])        :  (List[Int], List[String])
```

The two readings are exactly those of the two transformer stacks. With `writer`
*inside* `reify`, each nondeterministic branch carries its own log, and the result
is a list of result-and-log pairs — the `List (Writer …)` order. With `writer`
*outside* `reify`, a single log threads through the whole search and we get one log
beside the list of results — the `Writer (List …)` order. The client code `pick`
is identical in both; we changed only the nesting of two handlers, and the
differing result *types* make the two orderings visibly distinct rather than
silently equal. 

This connects directly to the `ListT[IO]` finding from the modular-interpreters
chapter: there, fusing a list of order-sensitive effects into one `IO[List[A]]`
let re-association reorder them and broke the associativity law. Here the
corresponding interaction is *named* by handler order, the result types make the
two orderings different on their face, and there is no monadic `bind` whose
re-bracketing could quietly go wrong.

###Outlook: Multi-Prompt Delimited Continuations

In this section we used a single delimiter (reset) and a single control operator (shift). This is sufficient to implement monadic reflection for a single monad. However, modern effect systems often need several independently scoped effects that can coexist in the same program. For instance, consider a program with this shape:

```
handle State {
  handle Exception {
    put(1)
    raise("oops")
  }
}
```
The `put(1)` should be handled by the `State` handler and not the `Exception` handler. Multi-prompt delimited continuations provide separate control boundaries for these handlers, allowing operations to target the appropriate handler independently. Control operators are parameterized by a prompt and capture only up to the nearest enclosing occurrence of that prompt.
Conceptually, prompts act as names for different control effects. A state operation can target one prompt, while an exception operation targets another. This allows multiple effect handlers to coexist without interfering with one another.

For this reason, multi-prompt delimited continuations are often used as an implementation technique for algebraic effects and effect handlers. They provide the same basic mechanism as shift/reset, but with multiple independently addressable control boundaries.

## References

- A. Filinski, *Representing Monads*, POPL 1994. [doi:10.1145/174675.178047](https://doi.org/10.1145/174675.178047)
- A. Filinski, *Monads in Action*, POPL 2010.
- G. Plotkin and J. Power, *Algebraic Operations and Generic Effects*, Applied Categorical Structures, 2003.
- G. Plotkin and M. Pretnar, *Handlers of Algebraic Effects*, ESOP 2009.
- A. Bauer and M. Pretnar, *Programming with Algebraic Effects and Handlers*, JLAMP, 2015.
- J. I. Brachthäuser, P. Schuster, and K. Ostermann, *Effects as Capabilities: Effect Handlers and Lightweight Effect Polymorphism}*, OOPSLA 2020. See also [effekt-lang.org](https://effekt-lang.org).
