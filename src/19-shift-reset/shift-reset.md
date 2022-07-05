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
Delimited continuations are available in Racket and many variants of Scheme, OCaML, Haskell and,
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
in some variants of Scheme, including Racket, as an extension of Scala, as a library in OcaML, as well
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
            case ContV(k2) => eval(a, env, av => k(k2(av))) // compose continuations k2 and k
            case _ => sys.error("can only apply functions")
  })
  case Reset(e) => k(eval(e, env, x => x)) // reset the continuation to the identity function
  case Shift(param, body) => eval(body, env + (param -> ContV(k)), x => x)  // wrap current continuation and reset continuation
}
```

References:

  1) Olivier Danvy and Andre Filinski, “Abstracting Control, ” LISP and Functional Programming, 1990.
  2) O. Kiselyov, An argument against call/cc. http://okmij.org/ftp/continuations/against-callcc.html
  3) O. Kiselyov, Introduction to programming with shift and reset. http://okmij.org/ftp/continuations/#tutorial
