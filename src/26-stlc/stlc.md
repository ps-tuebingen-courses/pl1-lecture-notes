# Simply Typed Lambda Calculus

The content of this chapter is available as a Scala file [here.](./stlc.scala)

```scala mdoc:invisible
import scala.language.implicitConversions
```

The Simply Typed Lambda Calculus
================================

We will now consider the so-called _Simply Typed Lambda Calculus_ (STLC).
We start with the untyped substitution-based lambda calculus augmented by the possibility to add type annotations to function definitions.
The type annotation is ignored by the interpreter.
Why are we using the substitution-based interpreter? Because it is simpler to state the type soundness
theorem. If we had values that are separate from expressions, we would need to define a type system for
these values. This is particularly tricky for closures with their embedded environments.

We also show a couple of standard extensions to the simply typed lambda calculus:

  1. A unit type `JunitType()` with associated term `JUnit()`
  2. `Let`-bindings (which don't need type annotations)
  3. Type Ascription (corresponding to `:` in Scala, like `5 + 3: Int`)
  4. Products (or Pairs, more specifically)
  5. Sums (binary sums, more specifically)

To avoid the problem of "guessing" the "from" type in a function definition, we annotate
function definitions with the expected argument type.

```scala mdoc
sealed abstract class Type

enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, t: Type, body: Exp)
  case Ap(funExpr: Exp, argExpr: Exp)
  case Junit()
  case Let(x: String, xdef: Exp, body: Exp)
  case TypeAscription(e: Exp, t: Type)

  case Product(e1: Exp, e2: Exp)
  case Fst(e: Exp)
  case Snd(e: Exp)

  case SumLeft(left: Exp, rightType: Type)
  case SumRight(leftType: Type, right: Exp)
  case EliminateSum(e: Exp, fl: Exp, fr: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)

import Exp._

def freshName(names: Set[String], default: String): String = {
  var last: Int = 0
  var freshName = default
  while (names contains freshName) { freshName = default + last.toString; last += 1; }
  freshName
}

def freeVars(e: Exp): Set[String] = e match {
   case Id(x) => Set(x)
   case Add(l, r) => freeVars(l) ++ freeVars(r)
   case Fun(x, _, body) => freeVars(body) - x
   case Ap(f, a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
   case Junit() => Set.empty
   case TypeAscription(e, t) => freeVars(e)
   case Let(x, xdef, body) => freeVars(xdef) ++ (freeVars(body) - x)
   case Product(e1, e2) => freeVars(e1) ++ freeVars(e2)
   case Fst(e) => freeVars(e)
   case Snd(e) => freeVars(e)
   case SumLeft(e, _) => freeVars(e)
   case SumRight(_, e) => freeVars(e)
   case EliminateSum(e, fl, fr) => freeVars(e) ++ freeVars(fl) ++ freeVars(fr)
}

def subst(e1: Exp, x: String, e2: Exp): Exp = e1 match {
  case Num(n) => e1
  case Junit() => e1
  case Add(l, r) => Add(subst(l, x, e2), subst(r, x, e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case Ap(f, a) => Ap(subst(f, x, e2), subst(a, x, e2))
  case TypeAscription(e, t) => TypeAscription(subst(e, x, e2), t)
  case Fun(param, t, body) =>
    if (param == x) e1 else {
      val fvs = freeVars(body) ++ freeVars(e2)
      val newvar = freshName(fvs, param)
      Fun(newvar, t, subst(subst(body, param, Id(newvar)), x, e2))
    }
  case Let(y, ydef, body) =>
    if (x == y) Let(y, subst(ydef, x, e2), body) else {
      val fvs = freeVars(body) ++ freeVars(e2)
      val newvar = freshName(fvs, y)
      Let(newvar, subst(ydef, x, e2), subst(subst(body, y, Id(newvar)), x, e2))
    }
  case Product(a, b) => Product(subst(a, x, e2), subst(b, x, e2))
  case Fst(e) => Fst(subst(e, x, e2))
  case Snd(e) => Snd(subst(e, x, e2))
  case SumLeft(e, t) => SumLeft(subst(e, x, e2), t)
  case SumRight(t, e) => SumRight(t, subst(e, x, e2))
  case EliminateSum(e, fl, fr) =>
    EliminateSum(subst(e, x, e2), subst(fl, x, e2), subst(fr, x, e2))
}

def eval(e: Exp): Exp = e match {
  case Id(v) => sys.error("unbound identifier: " + v)
  case Add(l, r) => (eval(l), eval(r)) match {
    case (Num(x), Num(y)) => Num(x + y)
    case _ => sys.error("can only add numbers")
  }
  case Ap(f, a) => eval(f) match {
    case Fun(x, _, body) => eval(subst(body, x, eval(a))) // call-by-value
    case _ => sys.error("can only apply functions")
  }
  case TypeAscription(e, _) => eval(e)
  case Let(x, xdef, body) => eval(subst(body, x, eval(xdef)))
  case Product(a, b) => Product(eval(a), eval(b))
  case Fst(e) => eval(e) match {
    case Product(a, b) => a
    case _ => sys.error("can only apply Fst to products")
  }
  case Snd(e) => eval(e) match {
    case Product(a, b) => b
    case _ => sys.error("can only apply Snd to products")
  }
  case SumLeft(e, t) => SumLeft(eval(e), t)
  case SumRight(t, e) => SumRight(t, eval(e))
  case EliminateSum(e, fl, fr) => eval(e) match {
    case SumLeft(e2, _) => eval(Ap(fl, e2))
    case SumRight(_, e2) => eval(Ap(fr, e2))
    case _ => sys.error("can only eliminate sums")
  }
  case _ => e // numbers and functions evaluate to themselves
}
```

We classify values into three types: Booleans, integers, and function types. For function types, we need some abstraction for its input
and output; otherwise the type checker cannot be compositional. Luckily we do already have such an abstraction, namely types.
Hence ``FunType`` becomes a recursive data type.

```scala mdoc
case class NumType() extends Type
case class FunType(from: Type, to: Type) extends Type
case class JunitType() extends Type
case class ProductType(fst: Type, snd: Type) extends Type
case class SumType(left: Type, right: Type) extends Type
```

Let's look at the type checker for the simply typed lambda calculus. To deal with identifiers,
we need an abstraction of environments. A type environment has the form ``Map[String, Type]``.

The type checker for the STLC is as follows:

```scala mdoc
def typeCheck(e: Exp, gamma: Map[String, Type]): Type = e match {
  case Num(n) => NumType()
  case Junit() => JunitType()
  case Id(x) => gamma.get(x) match {
    case Some(t) => t
    case _ => sys.error("free variable: " ++ x.toString)
  }
  case Add(l, r) => (typeCheck(l, gamma), typeCheck(r, gamma)) match {
    case (NumType(), NumType()) => NumType()
    case _ => sys.error("Type error in Add")
  }
  case Fun(x, t, body) => FunType(t, typeCheck(body, gamma + (x -> t)))
  case Ap(f, a) => {
    typeCheck(f, gamma) match {
      case FunType(from, to) =>
        if (from == typeCheck(a, gamma))
          to
        else
          sys.error("type error: arg does not match expected type")
      case _ => sys.error("first operand of Ap must be a function")
    }
  }
  case Let(x, xdef, body) => typeCheck(body, gamma + (x -> typeCheck(xdef, gamma)))
  case TypeAscription(e, t) =>
    if (typeCheck(e, gamma) == t) t else sys.error("type error in ascription")
  case Product(e1, e2) => ProductType(typeCheck(e1, gamma), typeCheck(e2, gamma))
  case Fst(e) => typeCheck(e, gamma) match {
    case ProductType(t1, t2) => t1
    case _ => sys.error("can only project Products")
  }
  case Snd(e) => typeCheck(e, gamma) match {
    case ProductType(t1, t2) => t2
    case _ => sys.error("can only project Products")
  }
  case SumLeft(e, t) => SumType(typeCheck(e, gamma), t)
  case SumRight(t, e) => SumType(t, typeCheck(e, gamma))
  case EliminateSum(e, fl, fr) => typeCheck(e, gamma) match {
    case SumType(left, right) => (typeCheck(fl, gamma), typeCheck(fr, gamma)) match {
      case (FunType(lf, t1), FunType(rf, t2)) if ((left == lf) && (right == rf)) =>
        if (t1 == t2)
          t1
        else
          sys.error("type error: functions must have same return type")
      case _ =>
        sys.error("type error in EliminateSum: second and third argument must be functions")
    }
    case _ => sys.error("type error: can only eliminate sums")
  }

}
```

Soundness of Simply Typed Lambda Calculus (STLC):

> If `e: Exp` and `typeCheck(e, Map.empty) == t`, then `typeCheck(eval(e), Map.empty) == t`.

<!-- prevent questionnaire from showing up if there is no javascript enabled-->
<noscript><style>questionnaire { display: none; }</style></noscript>
<!-- warning for user - feel free to leave out or customize -->
<noscript><div>Enable JavaScript to see the quiz</div></noscript>

<questionnaire language="en">
  <question type="singlechoice">
    What is the overall type of the following expression?
    <pre><code class="language-scala">
  Fun("x", NumType(), Product(5, Add(3, "x")))  
    </code></pre>
    <distractor>
      <pre><code class="language-scala">
  FunType(NumType(), NumType())
      </code></pre>
      <explanation>The return type is a product.</explanation>
    </distractor>
    <distractor>
      <pre><code class="language-scala">
  ProductType(NumType(), NumType())
      </code></pre>
      <explanation>This is only the return type of the function.</explanation>
    </distractor>
    <solution>
      <pre><code class="language-scala">
  FunType(NumType(),
          ProductType(NumType(), NumType()))
      </code></pre>
    </solution>
    <distractor>
      <pre><code class="language-scala">
  NumType()
      </code></pre>
      <explanation>This is only the input type of the function.</explanation>
    </distractor>
  </question>
  <question type="singlechoice">
    What is the overall type of the following expression?
    <pre><code class="language-scala">
  EliminateSum(SumLeft(3, JunitType()),
               Fun("x", NumType(), "x"),
               Fun("x", JunitType(), 0))
    </code></pre>
    <distractor>
      <code class="language-scala">
      JunitType()
      </code>
      <explanation>This is the type of the right-hand side of the sum type.</explanation>
    </distractor>
    <solution>
      <code class="language-scala">
      NumType()
      </code>
    </solution>
    <distractor>
      <code class="language-scala">
      SumType(NumType(), JunitType())
      </code>
      <explanation>This is the type of the scrutinee.</explanation>
    </distractor>
    <distractor>
      <code class="language-scala">
      FunType(NumType(), NumType())
      </code>
      <explanation>This is the type of the whole left branch. The type of the elimination is the common return type of the branches.</explanation>
    </distractor>
  </question>
</questionnaire>
