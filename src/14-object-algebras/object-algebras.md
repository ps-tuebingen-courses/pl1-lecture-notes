# Object Algebras
The content of this chapter is available as a Scala file [here.](./object-algebras.scala)

```scala mdoc:invisible
import scala.language.implicitConversions
```

Object algebras: A practical way of using Church Encodings

In previous lectures, we have talked about the "expression problem": The problem of
encoding a data structure (like expressions) and functions on that data structure
(like evaluators, or pretty-printers) in such a way that both the data structure
and the functions thereon can be extended without modifying code, while at the same
time maintaining static type safety.

Church encodings look like an obscure theoretical idea at first, but we will see
that their incarnation as "object algebras" leads to a quite attractive and practical
solution to the expression problem.

Let's look at a Scala version of the Church encodings we saw for the lambda
calculus. We use objects instead of functions, but otherwise
it is the same. Let's start with booleans.

```scala mdoc
trait Bool {
  def ifthenelse[T](t: T, e: T) : T
}

case object T extends Bool {
  def ifthenelse[T](t: T, e: T) = t
}
case object F extends Bool {
  def ifthenelse[T](t: T, e: T) = e
}
def and(a: Bool, b: Bool) : Bool = a.ifthenelse(b,a)
```

In Smalltalk and related languages, booleans are actually implemented
like that (at least conceptually).

In the same way, we can encode Church numerals.

```scala mdoc
trait NumC {
  def fold[T](z: T, s: T => T) : T
}
case object Zero extends NumC {
  def fold[T](z: T, s: T => T) = z
}

case class Succ(n: NumC) extends NumC {
  def fold[T](z: T, s: T => T) = s(n.fold(z,s))
}

def plus(a: NumC, b: NumC) = {
  new NumC {
    def fold[T](z: T, s: T => T) : T = {
      a.fold(b.fold(z,s), s)
    }
  }
}
```

```scala mdoc:silent
val oneC = Succ(Zero)
val twoC = Succ(oneC)
val threeC = Succ(twoC)

def testplusC = plus(twoC,threeC).fold[Unit]( (), _ => print("."))
```


Object algebras are a different way to do Church encodings
in object-oriented languages. This is what the encoding
of Church numbers looks like in object algebra style.


```scala mdoc
trait NumSig[T] {
  def z : T
  def s(p: T) : T
}

trait Num {
  def apply[T](x: NumSig[T]) : T
}
```

In other words, every constructor of the data type is turned into
a function of the `NumSig` type, with recursive occurences being replaced
by the type constructor `T`.
Actual numbers have the type `Num`, i.e., they are basically functions of type
`NumSig[T] => T`.


In the terminology of universal algebra, `NumSig` is an algebraic
signature, and `NumSig[T] => T` is the type of algebras for that signature.
Compared to the Church encoding above, we bundle the arguments of the
`fold`-function into a trait `NumSig` and pass them as a single object.
The advantage of this encoding is that we can extend the set of parameters
of the `fold` by using inheritance.

In this encoding, the `plus`-function looks like this:

```scala mdoc
def plus(a: Num, b: Num) = new Num {
  def apply[T](x: NumSig[T]) : T = a( new NumSig[T] {
    def z = b(x)
    def s(p: T) = x.s(p)
  })
}
```

Here is the representation of some numbers.

```scala mdoc:silent
val zero : Num = new Num { def apply[T](x: NumSig[T]) = x.z }
val one : Num = new Num { def apply[T](x: NumSig[T]) = x.s(x.z) }
val two : Num = new Num { def apply[T](x: NumSig[T]) = x.s(one(x)) }
val three : Num = new Num { def apply[T](x: NumSig[T]) = x.s(two(x)) }
```

This is an interpretation of the `Num`-"language" as Scala integers:

```scala mdoc
object NumAlg extends NumSig[Int] {
  def z = 0
  def s(x : Int) = x + 1
}
```

```scala mdoc
val testplus = plus(two, three)(NumAlg)
```

Let's look at a more useful application of object algebras. We encode
expression trees as object algebras.

```scala mdoc
trait Exp[T] {
  implicit def id(name: String) : T
  def fun(param: String, body: T): T
  def ap(funExpr: T, argExpr: T) :T
  implicit def num(n: Int) : T
  def add(e1: T, e2: T) : T
  def wth(x: String, xdef: T, body: T) : T = ap(fun(x,body), xdef)
}
```

The structure of expressions forces compositional interpretations. Hence
we use the compositional interpretation using meta-level closures to represent
closures.

```scala mdoc
sealed abstract class Value
type Env = Map[String, Value]
case class ClosureV(f: Value => Value) extends Value
case class NumV(n: Int) extends Value
```

An interpretation of expressions is now an implementation of the `Exp` interface

```scala mdoc
trait eval extends Exp[Env => Value] {
  def id(name: String) = env => env(name)
  def fun(param: String, body: Env => Value) = env => ClosureV(v => body(env + (param -> v)))
  def ap(funExpr: Env => Value, argExpr: Env => Value) = env => funExpr(env) match {
    case ClosureV(f) => f(argExpr(env))
    case _ => sys.error("can only apply functions")
  }
  def num(n: Int) = env => NumV(n)
  def add(e1: Env => Value, e2: Env => Value) = env => (e1(env), e2(env)) match {
    case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
    case _ => sys.error("can only add numbers")
  }
}

object eval extends eval
```

An example program becomes a function that is parametric in the choosen interpretation:

```scala mdoc
def test[T](semantics : Exp[T]) = {
  import semantics._

  ap(ap(fun("x", fun("y", add("x", "y"))), 5), 3)
}
```

We evaluate the program by folding the `eval`-visitor over it.

```scala mdoc
val testres = test(eval)(Map.empty)
```

The object algebra encoding of expressions is quite extensible. For instance, we can
add another case to the expression data type by extending the interface.

```scala mdoc
trait ExpWithMult[T] extends Exp[T] {
  def mult(e1: T, e2: T) : T
}

trait evalWithMult extends eval with ExpWithMult[Env => Value] {
  def mult(e1: Env => Value, e2: Env => Value) = env => (e1(env), e2(env)) match {
    case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
    case _ => sys.error("can only multiply numbers")
  }
}
object evalWithMult extends evalWithMult

def testMult[T](semantics : ExpWithMult[T]) = {
  import semantics._

  ap(ap(fun("x", fun("y", mult("x", "y"))), 5), 3)
}
```

```scala mdoc
val testresMult = testMult(evalWithMult)(Map.empty)
```

Note that there is no danger of confusing the language variants. For instance,
an attempt to pass `testMult` to `eval` will be a static type error.

At the same time, we can add another function on expressions (such as a pretty-printer)
without changing existing code, too: Just add a 
`trait prettyPrint extends Exp[String]` and, if pretty-printing of `ExpWithMult`
is required, extend with `trait prettyPrintMult extends prettyPrint with ExpWithMult[String]`.
This is the object algebra way of solving the expression problem.

We can also go one step further and combine object algebras with
typed higher-order abstract syntax, using higher-kinded type members.
Don't panic if you don't understand what is going on here.

```scala mdoc
trait ExpT {
  type Rep[_]
  def fun[S,T](f: Rep[S] => Rep[T]): Rep[S => T]
  def ap[S,T](funExpr: Rep[S => T], argExpr: Rep[S]) : Rep[T]
  implicit def num(n: Int) : Rep[Int]
  def add(e1: Rep[Int], e2: Rep[Int]) : Rep[Int]
}
```

Instead of having the semantic domain as a type parameter `T` as above, 
we encode it as a higher-kinded abstract type member `Rep[_]`. This leads
to significantly shorter type signatures and more fine-grained typing 
via Scala's support for "path-dependent types".

Note that, in contrast to `eval`, no dynamic checks (`match` ...) are needed
in the interpreter. Also, the result type of the evaluator is just `X`. In
contrast to `Value`, this is a "tagless" type, that is, it does not maintain
information at runtime about which variant it is. This is because the `ExpT`
datatype guarantees well-typedness of expressions. This interpreter will, in
general, run much faster and can in fact be formulated in such a way that
interpreting a program is just as fast as writing the program directly in
the meta language.

```scala mdoc
object evalT extends ExpT {
  type Rep[X] = X
  def fun[S,T](f: S => T) = f
  def ap[S,T](f: S => T, a: S) = f(a)
  def num(n: Int) = n
  def add(e1: Int, e2: Int) = e1 + e2
}

object prettyprintT extends ExpT {
  var counter = 0
  type Rep[X] = String
  def fun[S,T](f: String => String) = {
    val varname = "x" + counter.toString
    counter += 1
    "(" + varname + " => " +  f("x" + varname) + ")"
  }
  def ap[S,T](f: String, a: String) = f + "(" + a + ")"
  def num(n: Int) = n.toString
  def add(e1: String, e2: String) = "(" + e1 + "+" + e2 + ")"
}

def test2(semantics: ExpT) = {
  import semantics._
  ap(ap(fun((x: Rep[Int]) => fun((y: Rep[Int]) => add(x, y))), 5), 3)
}
```

```scala mdoc
val testres2 = test2(evalT)
val testres3 = test2(prettyprintT)
```

An attempt to construct an ill-formed object program will be detected by
the Scala type checker. For instance, the following program which adds
a number and a function is rejected by the Scala type checker:

```scala mdoc:fail
def testilltyped(semantics: ExpT) = {
  import semantics._
  add(5, fun((x: Rep[Int]) => x))
}
```

The type system encoded in the `ExpT` type is the so-called "simply-typed lambda calculus". 
Encoding more expressive type system in a similar style (including, e.g., type parameters) 
is an active area of research. 

References:
B. Olivira, W.Cook. Extensibility for the Masses: Practical Extensibility with Object Algebras. Proc. ECOOP 2012.
