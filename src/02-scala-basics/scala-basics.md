# Scala Basics

The content of this chapter is available as a Scala file [here.](./scala-basics.scala)

This chapter is a whirlwind tour of the small subset of [Scala](https://www.scala-lang.org/)
that we use throughout these notes. It is *not* a general Scala tutorial: it assumes that you
already know how to program in some other language, and it only introduces the handful of
features we actually rely on — `val`, `def`, and `var`; classes, traits, and objects; case
classes and `enum`s; and pattern matching. We finish with a tiny interpreter that is the
blueprint for essentially everything that follows.

All Scala code in these notes is compiled and executed while the notes are being built (using
[mdoc](https://scalameta.org/mdoc/)). Wherever you see a `// ...` comment underneath a piece of
code, that is the *actual* output the compiler produced — the inferred type of a definition, or
the value and type of an expression.

## Values, methods, and variables

A `val` introduces an *immutable* binding: a name for a value that, once assigned, never changes.

```scala mdoc
val x = 2 + 2
```

Note that we did not have to state the type of `x`; Scala *infers* that it is an `Int`. We could
have written the type explicitly. In Scala, the type *follows* the name, separated by a colon:

```scala mdoc
val y: Int = 2 + 2
```

(Semicolons at the end of a line are optional, and we usually omit them.)

A `def` defines a method (or function). Everything to the left of the `=` is the signature; the
expression to its right is the body. That body is just an expression, and its value is what the
method returns — idiomatic Scala has no `return` keyword.

```scala mdoc
def square(n: Int): Int = n * n

square(5)
```

Finally, a `var` is a *mutable* variable, whose binding can be reassigned later with `=`:

```scala mdoc
var counter = 0
counter = counter + 1
```

We will use `var` only rarely: almost all of the code in these notes is purely functional and
sticks to `val`.

## Classes, traits, and objects

Classes work much as in other object-oriented languages. Constructor parameters are written
directly after the class name; prefixing a parameter with `val` additionally exposes it as a
public field.

```scala mdoc
abstract class Person(val name: String) {
  def sayHello: Unit = print("hello, " + name)
}
```

A `trait` is like an interface, except that it may also provide concrete members. Traits are
Scala's unit of *mixin composition*.

```scala mdoc
trait ByeBye {
  def sayGoodbye: Unit = print("bye")
}
```

A class `extends` at most one superclass but may mix in any number of traits using `with`.
Inherited members are overridden with `override`.

```scala mdoc
class Tillmann(name: String) extends Person(name) with ByeBye {
  override def sayHello: Unit = print("hi")
}
```

An `object` defines a *singleton*: a class with exactly one instance, created lazily on first
use. Objects also serve as Scala's top-level, module-like namespaces — there are no `static`
members, you put such things into an object instead.

```scala mdoc
object Me extends Tillmann("Tillmann")

Me.sayHello
```

## Case classes and pattern matching

Now to the part that matters most for us. A `case class` is a class tailored to representing
immutable data. Compared to a plain class, the compiler additionally

  - makes every constructor parameter a `val` field,
  - lets us construct instances without `new`,
  - provides structural equality and a readable `toString`, and
  - allows an instance to be taken *apart* again by pattern matching.

We typically combine several case classes under a common supertype to model a fixed set of
alternatives. Marking that supertype `sealed` promises the compiler that *all* alternatives are
declared in this file, which in turn lets it warn us about incomplete pattern matches.

```scala mdoc
sealed trait UniPerson
case class Student(id: Int) extends UniPerson
case class Professor(subject: String) extends UniPerson
```

A `match` expression inspects a value and picks the first branch whose *pattern* fits, binding
the constructor arguments to names along the way:

```scala mdoc
def describe(p: UniPerson): String =
  p match {
    case Student(id)      => "student nr " + id
    case Professor(field) => "professor of " + field
  }

describe(Student(123))
describe(Professor("programming languages"))
```

Because `UniPerson` is `sealed` and the `match` covers both cases, the compiler is satisfied that
the match is exhaustive. Had we forgotten the `Professor` case, we would have gotten a warning.

## Enums

Declaring a sealed supertype together with one case class per alternative is such a common
pattern that Scala 3 provides dedicated syntax for it: an `enum`. The following declaration is
equivalent to the `UniPerson` hierarchy above, only more concise.

```scala mdoc
enum UniPersonEnum:
  case StudentEnum(id: Int)
  case ProfessorEnum(subject: String)
```

The cases live inside the enum's namespace, so we `import` them before use. Pattern matching then
works exactly as it did for case classes:

```scala mdoc
import UniPersonEnum._

def describeEnum(p: UniPersonEnum): String =
  p match {
    case StudentEnum(id)      => "student nr " + id
    case ProfessorEnum(field) => "professor of " + field
  }

describeEnum(StudentEnum(123))
```

An `enum` is automatically sealed, so we again get exhaustiveness checking for free. Throughout
these notes we use `enum`s to describe the *abstract syntax* of the little languages we study.

## A tiny interpreter

We now have everything we need to write the kind of program that recurs on almost every page of
these notes: an *interpreter* for a small language.

Consider a minimal language of arithmetic expressions. An expression is either a number, or the
addition of two subexpressions — a recursive, tree-shaped datatype, which we capture directly as
an `enum`. We wrap the whole example in an `object` just to keep its names together.

```scala mdoc
object AE {
  // Abstract syntax: an expression is a number, or the addition of two expressions.
  enum Exp:
    case Num(n: Int)
    case Add(lhs: Exp, rhs: Exp)

  import Exp._

  // An example expression, standing for 1 + (5 + 3):
  val onePlusEight = Add(Num(1), Add(Num(5), Num(3)))

  // The interpreter: recurse over the structure of the expression.
  def eval(e: Exp): Int =
    e match {
      case Num(n)        => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
    }
}
```

Notice how an expression is just an ordinary Scala value built from constructors — no parser and
no `new` in sight:

```scala mdoc
AE.onePlusEight
```

And here is the interpreter in action:

```scala mdoc
AE.eval(AE.onePlusEight)
```

Look closely at the shape of `eval`: there is exactly one `case` per constructor of `Exp`, and a
recursive call for every subexpression. The recursion structure of the interpreter mirrors the
recursive structure of the datatype it walks over. Almost every evaluator, type checker, and
program transformation in the rest of these notes has precisely this shape — an `enum` for the
abstract syntax of a language, and a recursive `def` that pattern-matches over it to give that
language a meaning.
