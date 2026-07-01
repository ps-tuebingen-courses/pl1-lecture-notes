# Bidirectional Type Checking

The content of this chapter is available as a Scala file [here.](./bidirectional-type-checking.scala)

```scala mdoc:invisible
import scala.language.implicitConversions
```

Bidirectional Type Checking
===========================

In the previous chapter on the Simply Typed Lambda Calculus (STLC) we paid a price for having
a type checker at all: almost every place where a new variable is introduced had to be annotated
with its type. Function definitions carried the argument type,

```scala
case Fun(param: String, t: Type, body: Exp)
```

and even the two sum injections had to mention the type of the summand that is _not_ present:

```scala
case SumLeft(left: Exp, rightType: Type)
case SumRight(leftType: Type, right: Exp)
```

Why were these annotations necessary? Recall how the STLC type checker worked: it was a single
function `typeCheck(e, gamma)` that, given an expression, _computes_ its type. Let us call this mode
of operation _synthesis_: information flows out of the expression. To synthesize the type of
`Fun("x", body)` we must know the type of `x`, but there is nothing inside the bare parameter `x`
that could tell us — so we asked the programmer to write it down. The same is true for `SumLeft(3)`:
the value `3` only tells us that the left component is a number, but the injection `Left 3` inhabits
`Num + T` for _every_ type `T`. Synthesis alone cannot invent that `T`, so we annotated it.

In the next chapter we will go to the opposite extreme and remove _all_ annotations, recovering the
missing types by generating fresh type variables and solving constraints with unification
(Hindley-Milner type inference). That is a powerful technique, but also a heavyweight one: it needs a
global constraint-solving phase, the occurs check, and the notion of a most general unifier. Worse,
for many interesting extensions of the type system — subtyping, higher-rank polymorphism, GADTs,
dependent types — full inference becomes undecidable or loses the property that every typable program
has a single "best" (principal) type.

_Bidirectional type checking_ is a lightweight middle ground that has become the standard tool for
exactly those richer systems. The idea is to keep the type checker syntax-directed and local — no
unification, no global constraints — but to be smarter about the _direction_ in which type
information flows. We split the single judgement into two mutually recursive modes:

  - **Synthesis** (also called _inference_), written \\( e \Rightarrow t \\): "given the expression
    `e`, compute its type `t`". Here `t` is an _output_. This is what the STLC checker did everywhere.
  - **Checking**, written \\( e \Leftarrow t \\): "given the expression `e` _and_ an expected type `t`,
    verify that `e` has type `t`". Here `t` is an _input_, supplied by the surrounding context.

The trick is that many expressions that cannot _synthesize_ a type can perfectly well be _checked_
against a known one. An unannotated function `Fun("x", body)` cannot say what its type is, but if the
context already expects it to have type `A -> B`, then we know `x : A` and can go on to check the body
against `B`. Likewise, `SumLeft(3)` cannot synthesize, but checked against `Num + T` it is fine for
any `T` we are handed. By threading expected types _inward_ we can delete precisely the annotations
that STLC forced upon us, while keeping the algorithm decidable, syntax-directed, and easy to give
good error messages for.

Which forms go into which mode? The governing principle is about where a type can come from:

  - A form can **synthesize** when its type is uniquely determined by the types of its immediate
    sub-expressions (together with the head constructor). The elimination forms are the typical case —
    application and projection consume something whose type we synthesize and read the result type off
    it — as are the self-describing leaves (a numeric literal, the unit value, a variable looked up in
    the environment).
  - A form must be **checked** when it would otherwise have to _invent_ a type that its sub-expressions
    do not pin down. The two archetypes are the function and the sum injection: a bare `Fun("x", body)`
    says nothing about the type of `x`, and `SumLeft(3)` leaves the right summand completely free
    (`Left 3` inhabits `Num + T` for _any_ `T`). The missing type is exactly what an expected type
    supplies.

This is often compressed into the slogan _"introduction forms are checked, elimination forms
synthesize"_, which matches the distinction between _normal_ and _neutral_ terms in natural deduction.
The slogan is a good default, but it is a heuristic rather than a law, and it is worth seeing where it
bends. Products are the standard example: a pair is an introduction form, yet it hides no information —
if `e1 ⇒ A` and `e2 ⇒ B` then unambiguously `Product(e1, e2) ⇒ A × B`. So we are free to give products a
synthesis rule as well, and we do; that is why `Product` appears in `infer` below. (Making pairs
check-only, forcing an ascription like `(⟨1, ()⟩ : Num × Junit)` whenever one occurs in a synthesizing
position, would be equally sound, just more verbose.) The exception also runs the other way: `EliminateSum`
is an _elimination_ form, but we will nonetheless make it check-only, because its result type is
determined by its branches rather than by the scrutinee it eliminates. The reliable question is always
"is the type recoverable from the sub-expressions?", not "is this an introduction or an elimination?".

Finally, we need two "bridges" between the modes:

  - From checking to synthesis: **type ascription**. An expression `(e : t)` synthesizes `t`, provided
    `e` checks against `t`. This is the only reason the language still has type ascriptions — they are
    now load-bearing, because they are the one construct that lets a check-only term (a bare lambda, an
    injection) be used in a position where a type must be synthesized.
  - From synthesis to checking: **subsumption**. To check an arbitrary expression against `t`, we may
    synthesize its type `t'` and confirm that `t' = t`. (In a system with subtyping this becomes
    `t' <: t`, which is where the name "subsumption" comes from.)

The syntax
----------

The syntax is that of the STLC, with the three annotations removed. Note that `TypeAscription` stays;
it is now the workhorse that connects the two modes.

```scala mdoc
sealed abstract class Type

enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp)           // no type annotation (checked)
  case Ap(funExpr: Exp, argExpr: Exp)
  case Junit()
  case Let(x: String, xdef: Exp, body: Exp)
  case TypeAscription(e: Exp, t: Type)         // the bridge between the two modes

  case Product(e1: Exp, e2: Exp)
  case Fst(e: Exp)
  case Snd(e: Exp)

  case SumLeft(left: Exp)                       // no type annotation (checked)
  case SumRight(right: Exp)                     // no type annotation (checked)
  case EliminateSum(e: Exp, fl: Exp, fr: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)

import Exp._
```

The dynamic semantics is completely unaffected by the change: types were never consulted at runtime,
so `freeVars`, `subst` and `eval` are literally the STLC versions with the deleted annotation fields
removed. We list them here only to keep the chapter self-contained.

```scala mdoc:silent
def freshName(names: Set[String], default: String): String = {
  var last: Int = 0
  var freshName = default
  while (names contains freshName) { freshName = default + last.toString; last += 1; }
  freshName
}

def freeVars(e: Exp): Set[String] = e match {
  case Id(x) => Set(x)
  case Add(l, r) => freeVars(l) ++ freeVars(r)
  case Fun(x, body) => freeVars(body) - x
  case Ap(f, a) => freeVars(f) ++ freeVars(a)
  case Num(n) => Set.empty
  case Junit() => Set.empty
  case TypeAscription(e, t) => freeVars(e)
  case Let(x, xdef, body) => freeVars(xdef) ++ (freeVars(body) - x)
  case Product(e1, e2) => freeVars(e1) ++ freeVars(e2)
  case Fst(e) => freeVars(e)
  case Snd(e) => freeVars(e)
  case SumLeft(e) => freeVars(e)
  case SumRight(e) => freeVars(e)
  case EliminateSum(e, fl, fr) => freeVars(e) ++ freeVars(fl) ++ freeVars(fr)
}

def subst(e1: Exp, x: String, e2: Exp): Exp = e1 match {
  case Num(n) => e1
  case Junit() => e1
  case Add(l, r) => Add(subst(l, x, e2), subst(r, x, e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case Ap(f, a) => Ap(subst(f, x, e2), subst(a, x, e2))
  case TypeAscription(e, t) => TypeAscription(subst(e, x, e2), t)
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
  case Product(a, b) => Product(subst(a, x, e2), subst(b, x, e2))
  case Fst(e) => Fst(subst(e, x, e2))
  case Snd(e) => Snd(subst(e, x, e2))
  case SumLeft(e) => SumLeft(subst(e, x, e2))
  case SumRight(e) => SumRight(subst(e, x, e2))
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
    case Fun(x, body) => eval(subst(body, x, eval(a))) // call-by-value
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
  case SumLeft(e) => SumLeft(eval(e))
  case SumRight(e) => SumRight(eval(e))
  case EliminateSum(e, fl, fr) => eval(e) match {
    case SumLeft(e2)  => eval(Ap(fl, e2))
    case SumRight(e2) => eval(Ap(fr, e2))
    case _ => sys.error("can only eliminate sums")
  }
  case _ => e // numbers and functions evaluate to themselves
}
```

The types are unchanged from STLC:

```scala mdoc
case class NumType() extends Type
case class FunType(from: Type, to: Type) extends Type
case class JunitType() extends Type
case class ProductType(fst: Type, snd: Type) extends Type
case class SumType(left: Type, right: Type) extends Type
```

The bidirectional type checker
------------------------------

Instead of one function we now have two mutually recursive ones, one per mode. `infer` returns a type
(synthesis); `check` takes an expected type and returns nothing, throwing an error if the expression
does not have that type. Because they call each other, they must be defined together.

Reading the two functions side by side makes the pattern visible. `infer` handles everything whose type
its sub-expressions determine: the eliminations (`Ap`, `Fst`, `Snd`), the self-describing leaves
(`Num`, `Junit`, `Id`), the primitive `Add`, and those introduction-flavoured forms that hide no type —
`Product`, and `Let` (which simply synthesizes from its definition). The forms with genuinely missing
information — `Fun`, `SumLeft`, `SumRight` — have _no_ synthesis rule, and neither does `EliminateSum`,
whose result type is fixed by its branches rather than by its scrutinee. Reaching any of these in
`infer` is a request for an annotation.

```scala mdoc
def infer(e: Exp, gamma: Map[String, Type]): Type = e match {
  case Num(_)  => NumType()
  case Junit() => JunitType()
  case Id(x)   => gamma.getOrElse(x, sys.error("free variable: " + x))

  case Add(l, r) =>
    check(l, NumType(), gamma); check(r, NumType(), gamma); NumType()

  // Elimination form: synthesize the function's type, then CHECK the argument
  // against its domain. This is what lets an unannotated argument lambda work.
  case Ap(f, a) =>
    infer(f, gamma) match {
      case FunType(from, to) => check(a, from, gamma); to
      case t => sys.error("application of a non-function of type " + t)
    }

  // The bridge from checking to synthesis: the ascription supplies the type
  // against which a check-only term (e.g. a bare lambda) is checked.
  case TypeAscription(e, t) => check(e, t, gamma); t

  // Let synthesizes if its definition does; the binding's type is remembered.
  case Let(x, xdef, body) =>
    infer(body, gamma + (x -> infer(xdef, gamma)))

  // Products synthesize when both components synthesize.
  case Product(e1, e2) => ProductType(infer(e1, gamma), infer(e2, gamma))

  case Fst(e) => infer(e, gamma) match {
    case ProductType(a, _) => a
    case t => sys.error("Fst of a non-product of type " + t)
  }
  case Snd(e) => infer(e, gamma) match {
    case ProductType(_, b) => b
    case t => sys.error("Snd of a non-product of type " + t)
  }

  // The check-only forms have no synthesis rule: ask for an annotation.
  case Fun(_, _) =>
    sys.error("cannot synthesize the type of an unannotated function; add a type ascription")
  case SumLeft(_) =>
    sys.error("cannot synthesize the type of a Left injection; add a type ascription")
  case SumRight(_) =>
    sys.error("cannot synthesize the type of a Right injection; add a type ascription")
  case EliminateSum(_, _, _) =>
    sys.error("cannot synthesize the type of a case expression; add a type ascription")
}

def check(e: Exp, expected: Type, gamma: Map[String, Type]): Unit = e match {
  // Unannotated lambda: recover the parameter type from the expected function type.
  case Fun(x, body) => expected match {
    case FunType(from, to) => check(body, to, gamma + (x -> from))
    case t => sys.error("a function must be checked against a function type, but got " + t)
  }

  // Injections recover the "other" summand from the expected sum type.
  case SumLeft(l) => expected match {
    case SumType(left, _) => check(l, left, gamma)
    case t => sys.error("a Left injection must be checked against a sum type, but got " + t)
  }
  case SumRight(r) => expected match {
    case SumType(_, right) => check(r, right, gamma)
    case t => sys.error("a Right injection must be checked against a sum type, but got " + t)
  }

  // Case analysis: synthesize the scrutinee's sum type, then check each branch
  // as a function into the EXPECTED result type. Branches may be bare lambdas;
  // their parameter types come from the scrutinee.
  case EliminateSum(scrut, fl, fr) => infer(scrut, gamma) match {
    case SumType(left, right) =>
      check(fl, FunType(left, expected), gamma)
      check(fr, FunType(right, expected), gamma)
    case t => sys.error("can only eliminate sums, but the scrutinee has type " + t)
  }

  // Propagating the expected type into a pair lets us check a pair of lambdas.
  case Product(e1, e2) => expected match {
    case ProductType(a, b) => check(e1, a, gamma); check(e2, b, gamma)
    case _ => subsume(e, expected, gamma)
  }

  // Let can pass the expected type on to its body.
  case Let(x, xdef, body) =>
    check(body, expected, gamma + (x -> infer(xdef, gamma)))

  // Subsumption: everything else synthesizes; infer and compare.
  case _ => subsume(e, expected, gamma)
}

def subsume(e: Exp, expected: Type, gamma: Map[String, Type]): Unit = {
  val actual = infer(e, gamma)
  if (actual != expected)
    sys.error("type mismatch: expected " + expected + " but inferred " + actual)
}
```

As in the STLC, the top-level entry point produces a type for a whole program; here that means running
it in synthesis mode:

```scala mdoc
def typeCheck(e: Exp, gamma: Map[String, Type]): Type = infer(e, gamma)
```

Examples
--------

An unannotated function has no type of its own, so on its own it cannot be type checked. We give it a
type with an ascription, which is where checking mode kicks in — the body is then checked against the
codomain with `x` bound to the domain type:

```scala mdoc
val idNum = TypeAscription(Fun("x", "x"), FunType(NumType(), NumType()))
typeCheck(idNum, Map.empty)
```

The pleasant part is that annotations are only needed where information is genuinely missing. A bare
lambda in _argument position_ needs no ascription at all, because `Ap` checks the argument against the
function's domain, and the expected type flows into the lambda from there:

```scala mdoc
val applyTo7 = TypeAscription(
  Fun("f", Ap("f", 7)),
  FunType(FunType(NumType(), NumType()), NumType()))

typeCheck(Ap(applyTo7, Fun("x", Add("x", 1))), Map.empty)
eval(Ap(applyTo7, Fun("x", Add("x", 1))))
```

Sum injections and case analysis behave the same way. `Left 3` is checked against a sum type supplied
by an ascription, and the `EliminateSum` — itself check-only — is checked against the expected result
type, from which each branch (again a bare lambda) receives its parameter and result types:

```scala mdoc
val leftVal = TypeAscription(SumLeft(3), SumType(NumType(), JunitType()))

val caseExpr =
  TypeAscription(
    EliminateSum(leftVal, Fun("n", Add("n", 1)), Fun("u", 0)),
    NumType())

typeCheck(caseExpr, Map.empty)
eval(caseExpr)
```

If we do ask the checker to synthesize the type of a bare function, it refuses — not because the
program is ill-typed, but because there is no annotation to recover the argument type from. This is the
one situation the design deliberately rules out, and the error message points straight at the fix:

```scala mdoc:crash
typeCheck(Fun("x", "x"), Map.empty)
```

Discussion
----------

The relationship to the two neighbouring chapters is worth making explicit. Compared to the STLC of the
previous chapter, bidirectional checking accepts exactly the same programs, but needs _fewer_
annotations: the argument type of a function and the tag of a sum injection are now recovered from
context, and an annotation is only required where an introduction form appears in a synthesizing
position (most visibly, a "redex" `Ap(Fun(...), a)` that applies a bare lambda directly). It does _not_,
however, buy us the extra expressiveness of the next chapter: there is still no let-polymorphism, since
that requires the type-variable inference we develop there.

The system remains sound in the same sense as the STLC:

> If `typeCheck(e, Map.empty) == t`, then `typeCheck(eval(e), Map.empty) == t`.

### A little history

The distinction between checking a term against a known type and synthesizing a type from a term is old
folklore, closely related to the difference between _normal_ and _neutral_ terms in natural deduction
(introduction forms are checked, elimination forms synthesize — the very recipe we used above).
It was turned into a named, practical technique by Benjamin Pierce and David Turner in their work on
_local type inference_ (POPL 1998; TOMS 2000), whose goal was precisely to recover annotations locally,
by propagating types between adjacent nodes of the syntax tree, instead of solving global constraints.
Around the same time the approach became a standard workhorse for systems where full inference is out
of reach: Thierry Coquand's 1996 algorithm for type-checking dependent types is an early instance, and
bidirectional formulations are pervasive in dependently typed languages and logical frameworks. More
recently, Dunfield and Krishnaswami's "Complete and Easy Bidirectional Typechecking for Higher-Rank
Polymorphism" (ICFP 2013) and their survey "Bidirectional Typing" (ACM Computing Surveys, 2021) have
made it the default presentation technique for modern type systems. The reason for its popularity is
always the same: it degrades gracefully. Add subtyping, higher-rank polymorphism, or dependent types,
and full inference may become undecidable or lose principal types — but a bidirectional checker stays
decidable, stays syntax-directed, and localizes both the required annotations and the error messages.

### In practical languages

You have almost certainly _experienced_ the two modes without naming them. Unlike the Hindley-Milner
languages of the ML/Haskell tradition, Scala does not perform global type inference; it performs _local_
type inference, which is bidirectional propagation under a different name. Consider

```scala
List(1, 2, 3).map(x => x + 1)
```

The lambda `x => x + 1` carries no parameter type, yet it compiles, because the expected type flows from
the signature of `map` into the lambda — this is checking mode, exactly like our `Ap` rule pushing the
domain type into an unannotated argument function. Now write the same lambda where nothing supplies an
expected type:

```scala
val f = x => x + 1     // error: missing parameter type
```

Here the compiler is in synthesis mode and, just like our `infer` on a bare `Fun`, has nothing to go on.
The remedies are precisely the two bridges from this chapter: give the context an expected type

```scala
val f: Int => Int = x => x + 1     // checking mode
```

(the analogue of our type ascription), or annotate the parameter directly, `val f = (x: Int) => x + 1`.
The same reasoning explains why Scala infers most method return types (it synthesizes them from the body)
but demands an explicit return type for a _recursive_ method — synthesis cannot see through the recursion,
so an annotation must break the cycle.

The pattern is everywhere once you look for it: TypeScript calls the checking direction _contextual
typing_ (parameter types of a function expression are taken from its expected type); Java and C# call it
_target typing_ for lambdas; Haskell needs explicit signatures exactly where it leaves the comfortable
world of full inference, for higher-rank types and GADTs; and Rust infers freely _inside_ a function body
while requiring signatures at function boundaries. In every case the same lesson from this chapter is at
work: by letting expected types flow inward, a language can drop most annotations while keeping type
checking simple, predictable, and local.

<!-- prevent questionnaire from showing up if there is no javascript enabled-->
<noscript><style>questionnaire { display: none; }</style></noscript>
<!-- warning for user - feel free to leave out or customize -->
<noscript><div>Enable JavaScript to see the quiz</div></noscript>

<questionnaire language="en">
  <question type="singlechoice">
    What happens when we evaluate the following?
    <pre><code class="language-scala">
  typeCheck(Fun("x", Add("x", 1)), Map.empty)
    </code></pre>
    <distractor>
      <pre><code class="language-scala">
  FunType(NumType(), NumType())
      </code></pre>
      <explanation>That would require inferring the argument type. The bidirectional checker only
      synthesizes when the information is present, and a bare, unannotated lambda in synthesizing
      position provides none.</explanation>
    </distractor>
    <distractor>
      <pre><code class="language-scala">
  NumType()
      </code></pre>
      <explanation>This is the type of the body once <code>x</code> is known to be a number, not the
      type of the whole function.</explanation>
    </distractor>
    <solution>
      It raises an error: the type of an unannotated function cannot be synthesized, so an
      ascription (or a checking context) is required.
    </solution>
    <distractor>
      It loops forever while guessing the argument type.
      <explanation>There is no guessing or search in bidirectional checking; it simply reports that a
      synthesis rule is missing.</explanation>
    </distractor>
  </question>
  <question type="multiplechoice">
    Which of the following expression forms are <em>checked</em> against an expected type rather than
    synthesizing a type of their own?
    <solution>
      <code class="language-scala">Fun(param, body)</code>
    </solution>
    <solution>
      <code class="language-scala">SumLeft(left)</code>
    </solution>
    <distractor>
      <code class="language-scala">Ap(funExpr, argExpr)</code>
      <explanation>Application is an elimination form: it synthesizes the function's type and checks
      the argument against the domain.</explanation>
    </distractor>
    <distractor>
      <code class="language-scala">Fst(e)</code>
      <explanation>Projection is an elimination form and synthesizes: it infers the product type and
      returns the first component type.</explanation>
    </distractor>
  </question>
</questionnaire>
