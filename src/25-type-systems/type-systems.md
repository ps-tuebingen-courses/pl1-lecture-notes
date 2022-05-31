# Type Systems

The content of this chapter is available as a Scala file [here.](./type-systems.scala)

```scala mdoc:invisible
import scala.language.implicitConversions
```
Type Systems
============

The aim of type systems is to prevent certain classes of errors to occur at runtime. Examples of such errors include the addition of
a number to a function, or the usage of a free variable. There can be many different type systems for the same programming language.
A sound type systems guarantees that the errors that the type system aims to prevent will not occur in _any_ run of the program.
This is different from testing, in which only a single program run on some particular input data is considered.

Since - by Rice's theorem - all interesting properties of the runtime behavior of a program (in a Turing complete language) are undecidable,
type systems are necessarily imperfect. But they are usually designed in such a way that the errors are directed:

Each type system unfairly rejects some programs that could be executed without runtime errors. But programs that are accepted by the
type system will never produce one of the precluded runtime errors. The latter property is usually called _soundness_,
the former property _completeness_. Hence we can say that most type systems are sound but not complete. One also often says that
type systems are _conservative_. A conservative approximation of the behavior of a program is one that considers all possible program
runs but also some impossible program runs - an overapproximation.

Well-typedness is usually a context-sensitive property, hence it cannot be expressed in the (context-free) grammar of the language
but is defined as an additional filter which rules out some syntactically correct programs.
Let's look at a small toy language to illustrate type systems:


```scala mdoc
enum Exp:
  case Num(n: Int)
  case Add(lhs: Exp, rhs: Exp)
  case Bool(x: Boolean)
  case If(cond: Exp, thenExp: Exp, els: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def bool2exp(x: Boolean): Exp = Bool(x)

import Exp._

def eval(e: Exp) : Exp = e match {
  case Add(l,r) => (eval(l), eval(r)) match {
                     case (Num(x),Num(y)) => Num(x+y)
                     case _ => sys.error("can only add numbers")
                    }
  case If(cond,thenExp,els) =>
    eval(cond) match {
      case Bool(true) => eval(thenExp)
      case Bool(false) => eval(els)
      case _ => sys.error("Condition must be boolean")
    }
  case _ => e
}
```

In this language, we can see that two different types of runtime errors can occur:
An addition where one of the operands is not a number, or an ``if``-expression where the condition
does not evaluate to a boolean.

Here are a few syntactically correct programs. Only the first three execute correctly. The
other ones yield runtime errors.

```scala mdoc:silent
val ex1 = Add(3,5)
val ex2 = If(true, 7, 5)
val ex2b = If(true, 7, true)
val ex3 = If(Add(3,4), 7, true)
val ex4 = Add(true,3)
```

Let's now design a type system for this language. Most type systems entail a classification of the values into different types.
In this type system, we choose to define two types, one called BoolType and one called IntType.


```scala mdoc
enum Type:
  case BoolType()
  case IntType()
import Type._
```

A type checker is a compositional assignment of types (or type errors) to expressions. Compositionality means that the type of a
composite expression is computed from the type of its subexpressions. Compositionality is quite important for reasoning about types.
In this case, the type checker is a rather straightforward structural recursion ( = compositionality) over expressions.

```scala mdoc
def typeCheck(e: Exp) : Type = e match {
  case Num(n) => IntType()
  case Bool(x) => BoolType()
  case Add(a,b) => (typeCheck(a),typeCheck(b)) match {
    case (IntType(),IntType()) => IntType()
    case _ => sys.error("Type Error in addition")
  }
  case If(c,t,e) => (typeCheck(c), typeCheck(t), typeCheck(e)) match {
    case (BoolType(),t1,t2) => if (t1 == t2) t1 else sys.error("type error in if")
    case _ => sys.error("Type error in If")
  }
}
```

## Type Soundness

If we typecheck the examples from above with the algorithm, we can see that it rejects all programs that lead to runtime errors,
and it also (unfairly) rejects one program that can be executed without error. Hence the type system is conservative, as expected.
We can now say more precisely what we mean by soundness of the type checker, namely that it correctly predicts the kind of value that
the interpreter will produce:

If ``e`` is an expression and ``typeCheck(e)`` yields a type ``t``, thenExp ``eval(e)`` yields a value ``v`` such that ``typeCheck(v) == t``.
This statement of type soundness has to be changed a bit when we have a programming language with non-terminating programs.
Then it will have the following form:

If ``e`` is an expression and ``typeCheck(e)`` yields a type ``t``, thenExp ``eval(e)`` either diverges, or it yields a value ``v``
such that ``typeCheck(v) == t``, but it does not terminate with a runtime error.
In the most general case, we also have runtime errors that are not captured by the type system. In this case we have to restrict
the last part of the sentence to those runtime errors that are captured by the type system.
