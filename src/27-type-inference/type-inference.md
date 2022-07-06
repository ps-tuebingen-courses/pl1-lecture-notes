# Type Inference

The content of this chapter is available as a Scala file [here.](./type-inference.scala)

```scala mdoc:invisible
import scala.language.implicitConversions
```
Type inference (sometimes called type reconstruction) is the idea to avoid type annotations
in the program and instead infer all typing-related information from the program.
For instance, if ``+`` is an operator on numbers, then we can infer that ``x`` must have
the number type in an expression ``x+1``.

The best-known type inference algorithm is Hindley-Milner type inference with so-called "let-polymorphism".
The basic idea of this algorithm is that the type checker synthesizes fresh type variables
whenever a type that is not yet known is needed as an input to a recursive call of the type
checking algorithm. For instance, in a function definition, a fresh type variable is created for the type fo the function argument. In addition to producing a type as output, the type checker will also produce
a set of equality constraints between types (containing type variables). Type checking succeeds only if these equality constraints have a solution. "Having a solution" means that there is a _substitution_ (a mapping from type variables to types) that makes all equations trivial. For instance, the substitution that
substitutes ``X`` by ``Num`` and ``Y`` by ``Bool``is a solution for the constraints ``FunType(X,Bool) == FunType(Num,Y)``.

In the above code we choose a slightly different representation of substitution, namely as a function
that performs the substitution (with regard to our discussion of refunctionalization we could say
  that we refunctionalize the substitution type).

```scala mdoc
enum Type:
  case FunType(from: Type, to: Type)
  case NumType()
  case TypeVar(x: String)
import Type._

def freeTypeVars(t: Type): Set[String] = t match {
  case FunType(f, t) => freeTypeVars(f) ++ freeTypeVars(t)
  case NumType() => Set.empty
  case TypeVar(x) => Set(x)
}

def substitution(x: String, s: Type) = new Function[Type, Type] {
  def apply(t: Type) = t match {
    case FunType(from, to) => FunType(this(from), this(to))
    case NumType() => NumType()
    case TypeVar(y) => if (x == y) s else TypeVar(y)
  }
}
```
A substitution can be found (if it exists) by an algorithm called the
"Robinson unification algorithm":

```scala mdoc
def unify(eq: List[(Type, Type)]): Type => Type = eq match {
  case Nil => identity _
  case (NumType(), NumType()) :: rest => unify(rest)
  case (FunType(f1, t1), FunType(f2, t2)) :: rest => unify((f1, f2) :: (t1, t2) :: rest)
  case (TypeVar(x), TypeVar(y)) :: rest if x == y => unify(rest)
  case (TypeVar(x), t) :: rest => {
    if (freeTypeVars(t)(x)) sys.error(s"Occurs check: $x occurs in $t")
    val s = substitution(x, t)
    s.andThen(unify(rest.map(p => (s(p._1), s(p._2)))))
  }
  case (t, TypeVar(x)) :: rest => unify((TypeVar(x), t) :: rest)
  case (t1, t2) :: rest => sys.error(s"Cannot unify $t1 and $t2")
}
```
It is not easy to see that this algorithm terminates in all cases, but it does (ask yourself: why?).
It is both sound and complete. It returns the so-called "most general unifier".

Let us now apply this to a concrete language. Here is its definition:

```scala mdoc
enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp) // No type annotation!
  case Ap (funExpr: Exp, argExpr: Exp)
  case Let(x: String, xdef: Exp, body: Exp)

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

def freeVars(e: Exp): Set[String] =  e match {
   case Id(x) => Set(x)
   case Add(l, r) => freeVars(l) ++ freeVars(r)
   case Fun(x, body) => freeVars(body) - x
   case Ap(f, a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
   case Let(x, xdef, body) => freeVars(xdef) ++ (freeVars(body) - x)
}

def subst(e1: Exp, x: String, e2: Exp): Exp = e1 match {
  case Num(n) => e1
  case Add(l, r) => Add(subst(l, x, e2), subst(r, x, e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case Ap(f, a) => Ap(subst(f, x, e2), subst(a, x, e2))
  case Fun(param, body) =>
    if (param == x) e1 else {
      val fvs = freeVars(body) ++ freeVars(e2)
      val newvar = freshName(fvs, param)
      Fun(newvar, subst(subst(body, param, Id(newvar)), x, e2))
    }
  case Let(y, ydef, body) =>
    if (x == y) Let(y, subst(ydef, x, e2), body) else {
      val fvs = freeVars(body) ++ freeVars(e2)
      val newvar = freshName(fvs, y)
      Let(newvar, subst(ydef, x, e2), subst(subst(body, y, Id(newvar)), x, e2))
    }
}

def eval(e: Exp): Exp = e match {
  case Id(v) => sys.error("unbound identifier: " + v)
  case Add(l, r) => (eval(l), eval(r)) match {
    case (Num(x), Num(y)) => Num(x + y)
    case _ => sys.error("can only add numbers")
  }
  case Ap(f, a) => eval(f) match {
    case Fun(x, body) => eval(subst(body, x, eval(a))) // call-by-value
    case _ => sys.error("can only apply functions")
  }
  case Let(x, xdef, body) => eval(subst(body, x, eval(xdef)))
  case _ => e // numbers and functions evaluate to themselves
}

```

The type checker returns both a type (possibly containing type variables) and a list
of equality constraints. Note that the only way type checking can actually fail is
when encountering a free variable.

```scala mdoc
var tyvCount: Int = 0
def freshTypeVar: TypeVar = {
  tyvCount += 1
  TypeVar("X" + tyvCount.toString)
}

def typeCheck(e: Exp, gamma: Map[String, Type]): (List[(Type, Type)], Type) = e match {
  case Num(n) => (List.empty, NumType())
  case Id(x) => gamma.get(x) match {
    case Some(t) => (List.empty, t)
    case _ => sys.error("free variable: " ++ x.toString)
  }
  case Add(l, r) => (typeCheck(l, gamma), typeCheck(r, gamma)) match {
    case ((eq1, t1), (eq2, t2)) =>
      ((t1, NumType()) :: (t2, NumType()) :: (eq1 ++ eq2), NumType())
  }
  case Fun(x, body) => {
    val xtype = freshTypeVar
    val resbody = typeCheck(body, gamma + (x -> xtype))
    (resbody._1, FunType(xtype, resbody._2))
  }
  case Ap(f, a) => {
    val toType = freshTypeVar
    (typeCheck(f, gamma), typeCheck(a, gamma)) match {
      case ((eqs1, ft), (eqs2, at)) =>
        ((ft, FunType(at, toType)) :: (eqs1 ++ eqs2), toType)
    }
  }
  case Let(x, xdef, body) => {
    // important if x is not used in body
    val (constraints1, _) = typeCheck(xdef, gamma)
    // Let-Polymorphism! type check after substitution
    val (constraints2, typ) = typeCheck(subst(body, x, xdef), gamma)
    (constraints1 ++ constraints2, typ)
  }

}
```

Full type checking is completed only when the constraints have a solution.
This is captured by this definition.
```scala mdoc

def doTypeCheck(e: Exp, gamma: Map[String, Type]) = {
  val (constraints, resType) = typeCheck(e, gamma)
  unify(constraints)(resType)
}

assert(doTypeCheck(42, Map.empty) == NumType())
assert(doTypeCheck(Fun("x", Add("x", 1)), Map.empty) == FunType(NumType(), NumType()))
```

The "let"-case of the type checker implements a special extension of the type checking algorithm, which
allows one to use the same definition type-polymorphically at multiple types.

For instance, here the identity function is once applied to a function and once to a number:

```scala mdoc
val exId =
  doTypeCheck(
    Let("id", Fun("x", "x"), Ap(Ap("id", Fun("x", Add("x", 1))), Ap("id", 42))),
    Map.empty)
```
This function could not be type-checked in STLC.

This example, on the other hand, should trigger an occurs check error:

```scala mdoc:crash
val exOmega = doTypeCheck(Fun("x", Ap("x", "x")), Map.empty)
```

Hence `omega` cannot be type-checked in STLC. STLC has the remarkable property that all well-typed programs terminate.

Completeness of type inference:

> If there exist type annotations that make a program type-check in the STLC type checker,
> then the type inference will also be able to type-check the non-annotated version of the program.

Due to "let-polymorphism", this program also type-checks some programs that would be ill-formed in STLC

The type system is still sound:

> If `doTypeCheck(e, Map.empty) == t`, then `eval(e) == v` and `doTypeCheck(v) == t` (modulo \\( \alpha \\)-renaming of type variables).
