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
