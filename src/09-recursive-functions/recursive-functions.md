# Recursive Functions

The content of this chapter is available as a Scala file [here.](./recursive-functions.scala)

```scala mdoc:invisible
import scala.language.implicitConversions
```

## Recursion

Let's try to write a function that computes the sum of the first \\( n \\) integers. Let's pretend we do not know that the sum of the
first \\( n \\) integers is \\( n * (n + 1) / 2 \\) and instead compute the sum in a loop. Let's try to do this in FAE (with `If0`):

```scala
val sumattempt = wth("sum", Fun("n", If0("n", 0, Add("n", Ap("sum", Add("n", -1))))),
                   Ap("sum", 10))
```

`sumattempt` won't work and yield an unbound identifier error (why?). An alternative would be to use a variant of the
_Y_ combinator to support recursion properly, but today we want to talk about direct support for recursion. More specifically,
we want a language construct `Letrec` that is similar to `with`, except that the bound String can be used in the expression
the String is bound to:

```scala
Letrec(x: String, e: Exp, body: Exp)
```

With this new construct our language looks like this:

```scala mdoc
object Syntax {
  enum Exp:
    case Num(n: Int)
    case Id(name: String)
    case Add(lhs: Exp, rhs: Exp)
    case If0(cond: Exp, thenExp: Exp, elseExp: Exp)
    case Fun(param: String, body: Exp)
    case Ap(funExpr: Exp, argExpr: Exp)
    case Letrec(x: String, e: Exp, body: Exp)

  object Exp:
    implicit def num2exp(n: Int): Exp = Num(n)
    implicit def id2exp(s: String): Exp = Id(s)
    def wth(x: String, xdef: Exp, body: Exp): Exp = Ap(Fun(x, body), xdef)
}

import Syntax._
import Exp._
```

Using `Letrec`, our example can be expressed as follows.

```scala mdoc:silent
val sum = Letrec("sum", Fun("n", If0("n", 0, Add("n", Ap("sum", Add("n", -1))))),
            Ap("sum", 10))
```

Let's now consider the semantics of `Letrec`. Consider the evaluation of ``Letrec(x, e, body)`` in an environment ``env``.
What environment should we use to evaluate `e` and `body`, respectively? Using `env` for `e` will produce a ``ClosureV(Fun("n", ..."sum"'...), env)``,
and hence the environment when evaluating body will be ``envbody = env + (x -> ClosureV(Fun("n", ..."sum"...), env))``.

This is bad, because the ``env`` in the closure does not contain a binding for ``"sum"`` and hence the recursive invocation will fail.
The environment in the closure must contain a mapping for ``"sum"``. Hence `envbody` should look like

```
  envbody = env + (x -> ClosureV(Fun("n", ..."sum"...),
                                 env + ("sum" -> ClosureV(Fun("n", ..."sum"...), env)))
```

This looks better, but now the second closure contains an environment with no binding of ``"sum"``. What we need is an environment
that satisfies the equation:

```
  envbody == env + (x -> ClosureV(Fun("n", ..."sum"..), envbody))
```

Obviously, `envbody` must be `circular`. There are different ways to create such a circular environment. We will choose mutation to create
a circle. More specifically, we introduce a mutable pointer to a value ``class ValuePointer`` which will be initialized with a null pointer.

In the `Letrec` case, we put such a `ValuePointer` in the environment and evaluate the (recursive) expression in that environment.
Then we update the pointer with the result of evaluating that expression.

The only other change we need to make is to dereference a potential value pointer in the `Id` case. We deal with the necessary case
distinction by a polymorphic `value` method.
Due to the mutual recursion between `ValueHolder` and `Value` the definitions are put into an object.

```scala mdoc
object Values {
  trait ValueHolder {
    def value: Value
  }
  sealed abstract class Value extends ValueHolder { def value = this }
  case class ValuePointer(var v: Value) extends ValueHolder { def value = v }
  case class NumV(n: Int) extends Value
  case class ClosureV(f: Fun, env: Env) extends Value
  type Env = Map[String, ValueHolder]
}

import Values._  // such that we do not have to write Values.ValueHolder etc.
```


The interpreter is unchanged except for the additional `Letrec` case and the modified `Id` case.

```scala mdoc
def eval(e: Exp, env: Env): Value = e match {
  case Num(n: Int) => NumV(n)
  case Id(x) => env(x).value  // dereference potential ValuePointer
  case If0(cond, thenExp, elseExp) => eval(cond, env) match {
    case NumV(0) => eval(thenExp, env)
    case _ => eval(elseExp, env)
  }
  case Add(l, r) => {
    (eval(l, env), eval(r, env)) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case f@Fun(param, body) => ClosureV(f, env)
  case Ap(f, a) => eval(f, env) match {
    case ClosureV(f, closureEnv) => eval(f.body, closureEnv + (f.param -> eval(a, env)))
    case _ => sys.error("can only apply functions")
  }
  case Letrec(x, e, body) => {
    val vp = ValuePointer(null)  // initialize pointer with null
    val newenv = env + (x -> vp) // environment extended with the placeholder for evaluating e
    vp.v = eval(e, newenv)       // create the circle in the environment
    eval(body, newenv)           // evaluate body in circular environment
  }
}
```

The sum of numbers from 1 to 10 should be 55.

```scala mdoc:silent
assert(eval(sum, Map.empty) == NumV(55))

// These test cases were contributed by rzhxeo (Sebastian Py)
val func = Fun("n", If0("n", 0, Ap("func", Add("n", -1))))
val test1 = Letrec("func", func, Ap("func", 1))
val test2 = Letrec("func", Ap(Fun("notUsed", func), 0), Ap("func", 1))
assert(eval(test1, Map()) == NumV(0))
assert(eval(test2, Map()) == NumV(0))
```
