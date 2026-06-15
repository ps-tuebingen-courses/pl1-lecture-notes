# Delimited Continuations

The content of this chapter is available as a Scala file [here.](./shift-reset.scala)


The continuations we have seen so far represent the whole call-stack. Invoking a continuation
was not like a function call because continuations never return a result. Invoking
a continuation is more similar to a disciplined version of GOTO.

However, the fact that continuations never return and represent the full call-stack often
makes their use cumbersome. In particular, such continuations cannot be composed. The
non-composability of continuations is visible in the fact that applications using
first-class continuations often need to make use of mutable state.

This is different with _delimited continuations_. Delimited continuations only represent
a segement of the call-stack. Delimited continuations
behave like functions, that is, they return a value and are hence composable.

Delimited continuations have many quite powerful applications, ranging from advanced exception handling
to backtracking algorithms and probabilistic programming as well as so-called algebraic effects.
Delimited continuations are available in Racket and many variants of Scheme, OCaml, Haskell and,
thanks to work in our research group, in Java.

There are many different variants of delimited continuations, many of them dating back to the late 1980s and
early 1990s. One of the most common forms of delimited continuations is in the form of `reset` and `shift`,
proposed by Olivier Danvy and Andrzej Filinski in 1990. The first of these primitives, `reset e`, marks the
current stack frame and continues with `e`. An invocation of the second primitive, `shift k e`, reifies the
stack segment between the current stack frame and the closest stack frame marked by a `reset` as a function,
binds this function to `k`, and evaluates `e` (which can then call `k` zero or more times).

Their meaning can be understood via a code transformation.

```
reset (...A... shift k e ...B...)
 ; -->
with k = lambda x. reset (...A... x ...B...) :
  reset e

reset e  ; no invocation of shift inside e
 ; -->
e
```

In class, we will look at a couple of examples of using `shift` and `reset`. `shift` and `reset` are available
in some variants of Scheme, including Racket, as an extension of Scala, as a library in OCaml, as well
as various usually partial, incomplete or buggy simulations in other languages.

A definitional interpreter for delimited continuations is pretty simple.

```scala mdoc
enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp)
  case Ap(funExpr: Exp, argExpr: Exp)
  case Shift(param: String, body: Exp)
  case Reset(body: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)
```

```scala mdoc:invisible
import Exp._
```

```scala mdoc
sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value
case class ContV(f: Value => Value) extends Value

def eval(e: Exp, env: Env, k: Value => Value): Value = e match {
  case Num(n: Int) => k(NumV(n))
  case Id(x) => k(env(x))
  case Add(l, r) => {
    eval(l, env, lv =>
        eval(r, env, rv =>
          (lv, rv) match {
            case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
            case _ => sys.error("can only add numbers")
          }))
  }
  case f@Fun(param, body) => k(ClosureV(f, env))

  case Ap(f, a) => eval(f, env, cl => cl match {
            case ClosureV(f, closureEnv) => eval(a, env, av => eval(f.body, closureEnv + (f.param -> av), k))
            // compose continuations k2 and k
            case ContV(k2) => eval(a, env, av => k(k2(av)))
            case _ => sys.error("can only apply functions and continuations")
  })
  // reset the continuation to the identity function
  case Reset(e) => k(eval(e, env, x => x))
  // wrap current continuation and reset continuation
  case Shift(param, body) => eval(body, env + (param -> ContV(k)), x => x)
}
```

## Delimited versus Undelimited Continuations

The continuations captured by `let/cc` in the previous chapter and those captured by
`shift` differ in one essential way. A `let/cc` continuation is *undelimited*: it
represents the entire rest of the computation, all the way up to the top level of the
program. A `shift` continuation is *delimited*: it represents only the segment of the
computation between the `shift` and the closest enclosing `reset`. It is natural to ask
how the two relate, and whether each can be expressed in terms of the other.

### `let/cc` as a special case of `shift`/`reset`

In one direction the answer is simple: an undelimited continuation is just the special
case of a delimited one whose delimiter sits at the very top of the program. Imagine
wrapping the whole program in a single outermost `reset`:

```
reset ( ...the entire program... )
```

Relative to this delimiter, "the segment up to the closest `reset`" and "the whole rest
of the computation" coincide. So `let/cc` is essentially a `shift` whose enclosing
`reset` is fixed at the outermost level.

There is one wrinkle, and it is exactly the difference we started the chapter with. A
`shift` continuation is composable: it behaves like a function and returns a value when
invoked. A `let/cc` continuation never returns; invoking it abandons the current
computation (our "disciplined GOTO"). To recover this aborting behaviour, the reinstated
continuation has to throw away the context it is invoked in — which `shift` itself can
do, by capturing the surrounding continuation and ignoring it:

```
; assuming the whole program sits inside one outermost reset:
let/cc k e
  :=
shift k0 (k0 (                        ; k0 = composable "rest of the program"
  let k = lambda x. shift _ (k0 x)    ; make k aborting: discard the current context
  in e))
```

So `let/cc` can be expressed with `shift`/`reset` and nothing else.

To see the encoding in action, take the small program

```
1 + (let/cc k (10 * (k 5)))
```

Under the real `let/cc`, the captured continuation is `k = lambda x. 1 + x`; invoking
`(k 5)` abandons the pending `10 * []` multiplication and yields `1 + 5 = 6`. Now run the
encoding, wrapping everything in one `reset` and writing `k0` for the continuation
captured by the outer `shift`:

```
reset( 1 + shift k0 (k0 (let k = lambda x. shift _ (k0 x) in 10 * (k 5))) )
```

The outer `shift` captures the context `1 + []` as `k0 = lambda x. reset (1 + x)` and, by
the reduction rule from above, leaves its body directly under the prompt:

```
--> reset( k0 (let k = lambda x. shift _ (k0 x) in 10 * (k 5)) )
```

Evaluating the body binds `k` and reaches `(k 5) = shift _ (k0 5)`, whose surrounding
context up to the prompt is `k0 (10 * [])`:

```
    reset( k0 (10 * shift _ (k0 5)) )
```

This inner `shift` binds that context to `_` and, because the underscore is never used,
discards it — the `10 * []` and the pending `k0`-application simply vanish. This is the
abort:

```
--> reset( k0 5 )
  = reset( reset (1 + 5) )
  = 6
```

We obtain `6`, exactly as the real `let/cc` does, and the multiplication never happens.

It is worth checking that the `shift _` is doing real work and is *not* a no-op. Suppose
we had used the composable continuation directly (`k := k0`, dropping the `shift _`
wrapper). Then `(k 5)` would *return* its result into the `10 * []` frame instead of
discarding it:

```
    reset( k0 (10 * (k0 5)) )
  = reset( k0 (10 * reset (1 + 5)) )
  = reset( k0 (10 * 6) )
  = reset( reset (1 + 60) )
  = 61
```

The difference between `6` and `61` is precisely the difference between an abortive
(undelimited) and a composable (delimited) continuation. The point that usually causes
confusion is *where the body of a `shift` returns its value*: not to the position of the
`shift`, but to the nearest enclosing `reset`. So although `(k0 5)` does compute and
return a value, it returns it to the prompt and out of the program — not back into the
`10 * []` context, which has already been thrown away.

### One-shot versus multi-shot continuations

The encoding above is faithful for the *abortive* use of `let/cc`: capture the
continuation, and at some later point jump to it *once*, abandoning the current
computation. This is the escape-like idiom behind exception handling, and it is all that
many uses of `let/cc` require.

First-class continuations are more powerful than that, however. They are *multi-shot*: a
captured continuation is an ordinary value that may be stored and invoked *any number of
times*, each invocation resuming the captured computation afresh. This is what makes the
coroutine and generator examples of the previous chapter work — `yield` stores a
continuation and the scheduler jumps back into it later, repeatedly. A faithful encoding
of `let/cc` would have to support this too.

Here the single-prompt encoding falls short, for two related reasons. First, the
continuation `k0` only makes sense within the dynamic extent of the top-level `reset`;
once that prompt has been left — for instance, after the program has returned normally —
there is no prompt for a later `shift _` to abort to. Second, a continuation captured by
`shift` is *re-delimited* and *composable*: invoking it installs a fresh prompt and
returns a value, rather than performing an independent global jump. Our `shift _` patch
repairs this for the *single, final* jump out of a delimited computation, but it tacitly
assumes that this jump is the last thing that happens under the prompt. If `k` is invoked
more than once, or invoked after the computation has already produced a value, the
one-prompt picture breaks down and the encoded continuation stops behaving like a genuine
undelimited one.

Making the encoding multi-shot therefore needs more machinery. The essential problem is
that an undelimited continuation must capture and reinstate the *entire* control state,
repeatedly and independently — not just the segment up to one fixed prompt. Two routes
are available. One is to stop relying on a single static prompt and instead keep the
whole-program continuation in a mutable cell — the same *metacontinuation* device we use
in the opposite direction below — re-installing a fresh top-level `reset` and restoring
the saved global continuation on every invocation of `k`, so that each jump is
independent and repeatable. The other is to move to the more expressive operators
`shift0`/`control0` (or `control`/`prompt`), which, unlike `shift`/`reset`, can capture
*past* the nearest prompt and manipulate the metacontinuation directly — enough to
rebuild a true undelimited, multi-shot continuation. Either way, the lesson is that the
tidy slogan "undelimited = delimited at the top level" is the whole story only for
one-shot escapes; faithful multi-shot behaviour costs us, once again, either extra state
or a stronger control operator.

### `shift`/`reset` in terms of `let/cc`

The converse direction is also possible, but not for free: with `let/cc` *alone* and no
mutable state, `shift`/`reset` cannot be expressed at all. The following is Filinski's
construction ("Representing Monads", 1994), which adds a single mutable cell to the
Racket `let/cc` of the previous chapter. The cell, `mk`, holds the *metacontinuation*: a
function representing the context *outside* the nearest enclosing `reset` — exactly the
information an undelimited `let/cc` continuation cannot give us on its own.

```racket
; the single mutable cell: the metacontinuation (context outside the current reset)
(define mk (box (lambda (_) (error "shift used outside of any reset"))))

; hand the final answer of the current delimited computation to the metacontinuation
(define (abort v) ((unbox mk) v))

(define (reset thunk)
  (let ([m (unbox mk)])             ; remember the enclosing metacontinuation
    (let/cc k                       ; k : the (undelimited) context after this reset
      (set-box! mk
        (lambda (v)                 ; once the body finishes with answer v:
          (set-box! mk m)           ;   restore the enclosing metacontinuation
          (k v)))                   ;   and resume the context after the reset
      (abort (thunk)))))            ; run the body; its value flows into mk

(define (shift h)
  (let/cc k                         ; k : the undelimited context, from here upward
    (abort
      (h (lambda (v)                ; the reified, composable delimited continuation:
           (let/cc k2               ; k2 : the context that invokes it
             (let ([m (unbox mk)])
               (set-box! mk
                 (lambda (r)        ; when the re-run delimited context finishes with r:
                   (set-box! mk m)  ;   restore mk, and
                   (k2 r)))         ;   *return* r to the call site — this is what makes it compose
               (k v)))))))))        ; re-run the captured context with v
```

There are three moving parts. The cell `mk` plays the role of the prompt, and `abort`
hands the final answer of the current delimited computation to whatever lies outside the
nearest `reset`. `reset` opens a new delimited computation: it remembers the enclosing
metacontinuation `m`, grabs the (abortive) continuation `k` after the `reset` with
`let/cc`, installs a new metacontinuation that says "when the body is done, put `m` back
and resume `k`", and finally runs the body and aborts its value into that
metacontinuation. `shift` grabs the undelimited continuation `k` and hands the handler a
*reified* version of it; the interesting part is that invoking the reified continuation
routes its exit through the cell — it points `mk` at its own call site `k2` just before
the abortive jump `(k v)`, so that when the re-run delimited computation finishes, control
*returns* to the call site instead of escaping. An abortive `let/cc` continuation has
thereby been turned into a composable, returning one.

The two examples from the start of this chapter evaluate as expected:

```racket
(+ 3 (reset (lambda () (+ 1 (shift (lambda (k) (+ 4 (k 2))))))))         ; => 10
(+ (reset (lambda () (+ 2 (shift (lambda (k) (+ (k 1) (k 3))))))) 5)     ; => 13
```

The second is the real test: `k` is invoked twice, which works because Racket's `let/cc`
continuations are themselves multi-shot, and because `mk` is saved and restored around
each invocation so that the two calls do not clobber one another.

One protocol wrinkle is worth flagging. Because `reset` and `shift` here are ordinary
*functions* and Scheme is call-by-value, we cannot write `(reset e)` directly: the
argument `e` would be evaluated *before* `reset` is even called — outside the prompt, and
before `mk` has been set up — so any `shift` inside `e` would abort to the wrong
metacontinuation. We therefore *thunkify* the body and write `(reset (lambda () e))`;
similarly `shift` takes a handler `(lambda (k) e)` rather than a bare body with a free
`k`. This is purely a matter of controlling evaluation order, and it is exactly what the
`reset`/`shift` *macros* in Racket's `racket/control` library do for you behind the
scenes, recovering the `reset e` / `shift k e` surface syntax used elsewhere in this
chapter.

This asymmetry — one direction free, the other requiring a cell — is not accidental, and
we have already met its cause. Recall the observation at the beginning of this chapter
that programs using first-class, undelimited continuations "often need to make use of
mutable state"; indeed every interesting `let/cc` example in the previous chapter (the
debugger, the coroutines) relied on `set!`. The underlying reason is composability, and
it is visible directly in our definitional interpreter. Applying a captured continuation
there reads

```scala
case ContV(k2) => eval(a, env, av => k(k2(av)))   // compose k2 with the current continuation k
```

The captured (delimited) continuation `k2` is composed with the current continuation `k`,
and evaluation proceeds normally afterwards. An undelimited continuation would instead
discard `k` and continue as `k2(av)` — it never comes back. Because undelimited
continuations do not return, the only way to reuse "the rest after the jump" more than
once is to save it somewhere in advance, and with `let/cc` the only place available is
mutable state. Delimited continuations, being composable, can simply hold on to that
context as an ordinary function. This is one of the reasons sometimes given (see the note
by Kiselyov linked below) for regarding `shift`/`reset` as the more fundamental of the two
primitives.


<!-- prevent questionnaire from showing up if there is no javascript enabled-->
<noscript><style>questionnaire { display: none; }</style></noscript>
<!-- warning for user - feel free to leave out or customize -->
<noscript><div>Enable JavaScript to see the quiz</div></noscript>

<questionnaire language="en">
  <question type="singlechoice">
    What is the part of the computation captured by <code class="language-scala">Shift</code>
    in the following expression?
    <pre><code class="language-scala">
  Add(3,
    Reset(Add(1,
      Shift("k", Add(4, Ap("k", 2))))))
    </code></pre>
    <solution>
      <code class="language-scala">
      Add(1, -)
      </code>
    </solution>
    <distractor>
      <code class="language-scala">
      Add(3, -)
      </code>
      <explanation>The part inside of <code class="language-scala">Reset</code> is captured.</explanation>
    </distractor>
    <distractor>
      <code class="language-scala">
      Add(3, Add(1, -)
      </code>
      <explanation>Only the part inside of <code class="language-scala">Reset</code> is captured.</explanation>
    </distractor>
    <distractor>
      <code class="language-scala">
      Add(4, -)
      </code>
      <explanation>The part outside of <code class="language-scala">Shift</code> is captured.</explanation>
    </distractor>
  </question>
  <question type="singlechoice">
    What is the result of evaluating the following expression?
    <pre><code class="language-scala">
  Add(
    Reset(Add(2,
      Shift("k", Add(Ap("k", 1), Ap("k", 3))))),  
    5)
    </code></pre>
    <distractor>
      <code class="language-scala">
      Num(8)
      </code>
      <explanation>The part outside of <code class="language-scala">Reset</code> is not discarded.</explanation>
    </distractor>
    <solution>
      <code class="language-scala">
      Num(13)
      </code>
    </solution>
    <distractor>
      <code class="language-scala">
      Num(18)
      </code>
      <explanation>The part outside of <code class="language-scala">Reset</code> is not captured.</explanation>
    </distractor>
    <distractor>
      <code class="language-scala">
      sys.error("can only add numbers")
      </code>
      <explanation>Applying the captured continuation to a number results in a number.</explanation>
    </distractor>
  </question>
</questionnaire>

References:

  1) Olivier Danvy and Andre Filinski, [“Abstracting control”, LISP and Functional Programming, 1990.](https://dl.acm.org/doi/10.1145/91556.91622)
  2) O. Kiselyov, [An argument against call/cc.](http://okmij.org/ftp/continuations/against-callcc.html)
  3) O. Kiselyov, [Introduction to programming with shift and reset.](http://okmij.org/ftp/continuations/#tutorial)
