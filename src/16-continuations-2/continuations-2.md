# Continuations: Definition

The content of this chapter is available as a Scala file [here.](./continuations-2.scala)

```scala mdoc:invisible
import scala.language.implicitConversions
```

The goal now is to make the "web" (or rather, CPS) transformation which we applied informally
in the previous lecture formal.

In the previous lecture we have seen that we had to translate the following program:

```scala
println("The sum is: " + (inputNumber("First number:") + inputNumber("Second number")))
```

into this program:

```scala
webread_k("First number", (n) =>
  webread_k("Second number:", (m) =>
    webdisplay("The sum is: " + (n + m))))
```

This hand-translation is sufficient if this expression is the entire program.
If we wish to use it as a sub-expression in a larger program, this does not suffice,
because there may be a pending computation outside its own evaluation. For that
reason, the program has to send its result to a continuation instead of returning it:

```scala
k => webread_k("First number", (n) =>
       webread_k("Second number:", (m) =>
         webdisplay("The sum is: " + k(n + m))))
```

This version can be employed in the transformation of a larger program. In the special
case where this is the entire program, we can apply the transformed term to the identity
function to get the same result as the previous manual transformation.

In general, every term, when converted to CPS, will be have the following properties
(see O. Danvy, Three Steps for the CPS Transformation, 1991):

  1) The values of all intermediate applications are given a name.

  2) The evaluation of these applications is sequentialized based on a traversal of their
     abstract syntax tree.

  3) The results of the transformation are procedures that expect a continuation parameter -
     a lambda abstraction whose application to intermediate values yields the final result
     of the whole evaluation.

### Function application

Let us now look at the transformation of a function application. For instance, let us
consider the term

```scala
f(a)(g(b))
```

The transformation of the function argument, ``f(a)``, should be

```scala
f_k(a, fval => ...)
```

Similarly, the transformation of the argument position would be:

```scala
g_k(b, arg => ...)
```

Given these two values, `fval` and `arg`, we can now perform the application, like so:

```scala
k(fval(arg))
```

However, this will not work, because if ``fval`` is in CPS itself, it will not return.
Instead, `k` must be given as an argument to the function, like so:

```scala
k => f_k(a, fval => g_k(b, arg => fval(arg, k)))
```

Reading this sequentially, it says to evaluate the function expression, store its value in `fval`,
then evaluate the argument, store its value in `arg`, and finally invoke the function value on the argument.
This function's continuation is the outer continuation of the function application itself.

### Variables and constants

What about variables and constants? Since every term in CPS must be a function that consumes a continuation,
the constant is simply sent to the continuation.
For instance, the CPS transformation of the number 3 is ``k => k(3)``.

### Function definitions

What about function definitions, such as ``x => x``? Since every lambda expression is also a constant,
we might be tempted to use the same rule as above, i.e.,

```scala
k => k(x => x)
```

However, the transformation is more subtle. A function application invokes the function on two arguments, whereas
the original function ``x => x`` consumes only one. What is the second argument?

__Answer__: It is the _dynamic_ continuation, i.e., the continuation at the time of the function _application_
(as opposed to its definition). We cannot ignore this continuation: It is the stack active at the point
of function invocation, so we want to preserve it. This is in contrast to what we did with environments,
and more in line with our treatment of the store.

The transformed version hence reads:

```scala
k => k((x, dynk) => (k => k(x))(dynk))
```

which is equivalent (when the inner application finally happens) to:

```scala
k => k((x, dynk) => dynk(x))
```

This is a function that accepts a value and a dynamic continuation and sends the value to that continuation.


## Formal Definition of the CPS Transformation

We are now ready to write this transformation formally, as a source-to-source transformation. This transformation
could have the type `cps(e: Exp): Exp`, but we choose a different type for two reasons:

  1) In CPS we need function definitions and applications with two arguments instead of one. This could be addressed
     by adding new syntax.

  2) More importantly, we want the invariants of the CPS format to be clearly visible in the syntax definition of the
     result, most importantly the fact that all function applications are tail calls.

```scala mdoc
enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp)
  case Ap(funExpr: Exp, argExpr: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)
```

```scala mdoc:invisible
import Exp._
```

For CPS transformed terms, we define two different syntactic categories: Serious expressions (`SeriousExp`) and trivial expressions (`TrivExp`).
By "trivial", we mean terms that "return" and have no control effect - in our language these are terms that are different from applications of functions
or continuations (neither of which returns). Additions (`CPSAdd`) are also values in this regard: We assume addition
to be built-in and not requiring CPS-transformations.

The terminology of serious and trivial expressions was introduced by John C. Reynolds in his seminal 1972 paper on "Definitional interpreters for higher-order programming languages".

The syntax makes clear that all arguments of a function application are trivial - hence no nesting of applications
can occur. Furthermore, the syntax differentiates between defining an ordinary function (`CPSFun`) which, when translated,
gets an additional continuation parameter, and Continuation Functions (`CPSCont`), which are the result of the CPS
transformation. Correspondingly, we have two different forms of applications, `CPSContAp` and `CPSFunAp`.

Here is the formal definition:

```scala mdoc
sealed abstract class SeriousExp
abstract class TrivExp extends SeriousExp
case class CPSNum(n: Int) extends TrivExp
case class CPSAdd(l: CPSVar, r: CPSVar) extends TrivExp
case class CPSCont(v: String, body: SeriousExp) extends TrivExp
case class CPSFun(x: String, k: String, body: SeriousExp) extends TrivExp
case class CPSVar(x: String) extends TrivExp { override def toString = x.toString }
implicit def id2cpsexp(x: String): CPSVar = CPSVar(x)

case class CPSContAp(k: TrivExp, a: TrivExp) extends SeriousExp
// the arguments are even CPSVar and not only TrivExp!
case class CPSFunAp(f: CPSVar, a: CPSVar, k: CPSVar) extends SeriousExp
```

With these definitions, we are now ready to formalize the transformation described above.
There is one technical issue: We need to introduce new names for binders into our program, such as `"k"`.
We need to make sure that we do not accidentially capture existing names in the program.
For this reason we need our ``freshName`` machinery we introduced for
[FAE](../07-higher-order-functions/higher-order-functions.html).

```scala mdoc
def freeVars(e: Exp): Set[String] =  e match {
   case Id(x) => Set(x)
   case Add(l, r) => freeVars(l) ++ freeVars(r)
   case Fun(x, body) => freeVars(body) - x
   case Ap(f, a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
}

def freshName(names: Set[String], default: String): String = {
  var last: Int = 0
  var freshName = default
  while (names contains freshName) { freshName = default + last; last += 1; }
  freshName
}

def cps(e: Exp): CPSCont = e match {
   case Add(e1, e2) => {
     val k = freshName(freeVars(e), "k")
     val lv = freshName(freeVars(e2), "lv")
     CPSCont(k, CPSContAp(cps(e1), CPSCont(lv,
       CPSContAp(cps(e2), CPSCont("rv",
         CPSContAp(k, CPSAdd("rv", lv)))))))
   }
   case Fun(a, body) => {
     val k = freshName(freeVars(e), "k")
     val dynk = freshName(freeVars(e), "dynk")
     CPSCont(k, CPSContAp(k, CPSFun(a, dynk, CPSContAp(cps(body), dynk))))
   }
   case Ap(f, a) => {
     val k = freshName(freeVars(e), "k")
     val fval = freshName(freeVars(a), "fval")
     CPSCont(k, CPSContAp(cps(f), CPSCont(fval,
       CPSContAp(cps(a), CPSCont("aval",
         CPSFunAp(fval, "aval", k))))))
   }
   case Id(x) => {
     val k = freshName(freeVars(e), "k")
     CPSCont(k, CPSContAp(k, CPSVar(x)))
   }
   case Num(n) => {
     val k = freshName(freeVars(e), "k")
     CPSCont(k, CPSContAp("k", CPSNum(n)))
   }
}
```

This transformation is the so-called Fischer CPS transformation. There are many other CPS transformation algorithms.
The Fischer CPS transformation is nice because it is so simple and because it is defined as one simple structural
recursion over the AST. Its main disadvantage is the existence of so-called "administrative redexes".

An administrative redex is a function application whose operator is a "continuation lambda" - a lambda produced during
CPS transformation that was not in the original program. Such function applications can be computed immediately because
the function which is called is known.

For instance, ``cps(Add(2, 3))`` yields

```scala
CPSCont("k",
        CPSContAp(
          CPSCont("k",
                  CPSContAp("k", 2)),
          CPSCont("lv",
                  CPSContAp(
                    CPSCont("k",
                            CPSContAp("k", 3)),
                    CPSCont("rv",
                            CPSContAp("k", CPSAdd("rv", "lv")))))))
```

instead of

```scala
CPSCont("k", CPSContAp("k", CPSAdd(2, 3)))
```

Many more advanced CPS transformation algorithms try to avoid as many administrative redexes as possible.
