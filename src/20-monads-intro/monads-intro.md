# Introduction to Monads

The content of this chapter is available as a Scala file [here.](./monads-intro.scala)


```scala mdoc:invisible
import scala.language.higherKinds
import scala.language.reflectiveCalls
```

We have seen various patterns of function composition:

- The environment-passing style, in which an environment is passed down in recursive calls.
- The store-passing style, in which a store is threaded in and out of every computation.
- The continuation-passing style, in which every function call is a tail call.

Monads are way to abstract over such patterns of function composition.

Motivation
----------
Using monads, we can write code which can be parameterized to be in
one of the styles above (or many others).
Here is another common pattern of function composition. Suppose we have the following API of (nonsensical) functions:

```scala mdoc
def f(n: Int): String = "x"
def g(x: String): Boolean = x == "x"
def h(b: Boolean): Int = if (b) 27 else sys.error("error")

def clientCode = h(!g(f(27) + "z"))
```


Now suppose that these functions can possibly fail (say, because they involve remote communication). A common way to deal with such
failures is to use the `Option` datatype:

```scala mdoc
def fOp(n: Int): Option[String] = if (n < 100) Some("x") else None
def gOp(x: String): Option[Boolean] = Some(x == "x")
def hOp(b: Boolean): Option[Int] = if (b) Some(27) else None
```

However, now the `clientCode` must be changed rather dramatically:

```scala mdoc
def clientCodeOp =
  fOp(27) match {
    case Some(x) => gOp(x + "z") match {
        case Some(y) => hOp(!y)
        case None => None
      }
    case None => None
  }
```

We see a kind of pattern in this code. We have a value of type ``Option[A]``, but the next function we need to call requires an `A` and
produces an ``Option[B]``. If the ``Option[A]`` value is ``None``, then the whole computation produces ``None``. If it is ``Some(x)``
instead, we pass ``x`` to the function.

We can capture this pattern in the form of a function:

```scala mdoc
def bindOption[A, B](a: Option[A], f: A => Option[B]): Option[B] = a match {
  case Some(x) => f(x)
  case None => None
}
```

Using ``bindOption``, we can rewrite the code above as follows:

```scala mdoc
def clientCodeOpBind =
  bindOption(fOp(27), (x: String) =>
    bindOption(gOp(x + "z"), (y: Boolean) =>
      hOp(!y)))
```

Now suppose that our original client code was not ``h(!g(f(27) + "z"))``
but instead ``!g(f(27) + "z")``. How can we express this with `bindOption`? This
thing does not type check:

```scala
def clientCode =
  bindOption(f(27), (x: String) =>
    bindOption(g(x + "z"), (y: Boolean) =>
      !y))
```

One way to fix the situation is to insert a call to ``Some``, like so:

```scala mdoc
def clientCode2Op =
  bindOption(fOp(27), (x: String) =>
    bindOption(gOp(x + "z"), (y: Boolean) =>
      Some(!y)))
```

While this works, it is incompatible with our original goal of abstracting over the function composition pattern, because the
`Some` constructor exposes what kind of pattern we are currently dealing with. Hence let's abstract over it by adding a second
function ``unit`` to our function composition interface:

```scala mdoc
def unit[A](x: A): Option[A] = Some(x)

def clientCode2OpUnit =
  bindOption(fOp(27), (x: String) =>
    bindOption(gOp(x + "z"), (y: Boolean) =>
      unit(!y)))
```

This looks better, but the types of `unit` and `bindOption` (and also the name, but we can of course change that) still
reveal that we are dealing with the ``Option`` function-composition pattern. Let's abstract over the `Option` type constructor
by turning the type constructor into a parameter. The resulting triple (type constructor, `unit` function, `bind` function) is
called a _monad_. Certain conditions (the "monad laws") on ``unit`` and ``bind`` also need to hold to make it a true monad,
but we'll defer a discussion of these conditions until later.


The Monad Interface
-------------------

So here it is: The Monad interface.

```scala mdoc
trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def bind[A, B](m: M[A], f: A => M[B]): M[B]
  // The "monad laws":
  // 1) "unit" acts as a kind of neutral element of "bind", that is:
  //    1a) bind(unit(x), f) == f(x) and
  //    1b) bind(x, y => unit(y)) == x
  // 2) Bind enjoys an associativity property
  //     bind(bind(x, f), g) == bind(x, y => bind(f(y), g))
}
```

Using this interface, we can now make `clientCode` depend only on this interface, but no longer on the `Option` type:

```scala mdoc
def clientCode2Op(m: Monad[Option]) =
  m.bind(fOp(27), (x: String) =>
    m.bind(gOp(x + "z"), (y: Boolean) =>
      m.unit(!y)))
```

If the API is parametric in the monad, we can make the client code fully parametric, too. We model the monad object as an
implicit parameter to save the work of passing on the monad in every call.

```scala mdoc
def fM[M[_]](n: Int)(using m: Monad[M]): M[String] = sys.error("not implemented")
def gM[M[_]](x: String)(using m: Monad[M]): M[Boolean] = sys.error("not implemented")
def hM[M[_]](b: Boolean)(using m: Monad[M]): M[Int] = sys.error("not implemented")

def clientCode2[M[_]](using m: Monad[M]) =
  m.bind(fM(27), (x: String) =>
    m.bind(gM(x + "z"), (y: Boolean) =>
      m.unit(!y)))
```

For-Comprehension Syntax
------------------------

All these nested calls to `bind` can make the code hard to read. Luckily, there is a notation called "monad-comprehension" to make
monadic code look simpler. Monad-comprehensions are directly supported in Haskell and some other languages. In Scala, we can
piggy-back on the "for-comprehension" syntax instead.
A "for-comprehension" is usually used for lists and other collections. For instance:

```scala mdoc:silent
val l = List(List(1, 2), List(3, 4))
assert((for { x <- l; y <- x } yield y + 1) == List(2, 3, 4, 5))
```

The Scala compiler desugars the for-comprehension above into calls of the standard `map` and `flatMap` functions. That is, the above
for-comprehension is equivalent to:

```scala mdoc:silent
assert(l.flatMap(x => x.map(y => y + 1)) == List(2, 3, 4, 5))
```

We will make use of for-comprehension syntax by supporting both ``flatMap`` (which is like ``bind``) and ``map`` (which is like ``fmap``).
We support these functions by an implicit conversion to an object that supports these functions as follows:

```scala mdoc
extension [A, M[_]](m: M[A])(using mm: Monad[M])
  def map[B](f: A => B): M[B] = mm.bind(m, (x: A) => mm.unit(f(x)))
  def flatMap[B](f: A => M[B]): M[B] = mm.bind(m, f)
```

Using the new support for for-comprehension syntax, we can rewrite our client code as follows: Given the API from above,

```scala
def fOp(n: Int): Option[String] = if (n < 100) Some("x") else None
def gOp(x: String): Option[Boolean] = Some(x == "x")
def hOp(b: Boolean): Option[Int] = if (b) Some(27) else None
```

We can now rewrite this

```scala
def clientCode2Op(m: Monad[Option]) =
  m.bind(fOp(27), (x: String) =>
    m.bind(gOp(x + "z"), (y: Boolean) =>
      m.unit(!y)))
```

to this:

```scala mdoc
def clientCode2OpFor(using m: Monad[Option]) =
  for {
    x <- fOp(27)
    y <- gOp(x + "z")
  } yield !y
```


The Option Monad
----------------
Let's look at some concrete monads now. We have of course already seen one particular monad: The `Option` monad. This monad is also
sometimes called the `Maybe` monad.

```scala mdoc
object OptionMonad extends Monad[Option] {
  override def bind[A, B](a: Option[A], f: A => Option[B]): Option[B] =
    a match {
      case Some(x) => f(x)
      case None => None
    }
  override def unit[A](a: A) = Some(a)
}
```

We can now parameterize `clientCode` with `OptionMonad`.

```scala mdoc
def v: Option[Boolean] = clientCode2Op(OptionMonad)
```

Generic Functions for Monads
----------------------------
There are many other sensible monads. Before we discuss those, let us discuss whether there are useful functions that are generic
enough to be useful for many different monads. Here are some of these functions:


``fmap`` turns every function between ``A`` and ``B`` into a function between ``M[A]`` and ``M[B]``:

```scala mdoc
def fmap[M[_], A, B](f: A => B)(using m: Monad[M]): M[A] => M[B] =
  a => m.bind(a, (x: A) => m.unit(f(x)))
```

In fancy category theory terms, we can say that every monad is a functor.

``sequence`` composes a list of monadic values into a single monadic value which is a list.

```scala mdoc
def sequence[M[_], A](l: List[M[A]])(using m: Monad[M]): M[List[A]] = l match {
  case x :: xs => m.bind(x, (y: A) =>
    m.bind(sequence(xs), (ys: List[A]) =>
      m.unit(y :: ys)))
  case Nil => m.unit(List.empty)
}
```

``mapM`` composes ``sequence`` and the standard ``map`` function:

```scala mdoc
def mapM[M[_], A, B](f: A => M[B], l: List[A])(using m: Monad[M]): M[List[B]] =
  sequence(l.map(f))
```

``join`` is another useful function to unwrap a layer of monads.
In category theory, monads are defined via ``unit`` (denoted by the greek letter η)
and ``join`` (denoted μ) instead of ``unit`` and ``bind``. There are additional "naturality" and
"coherence conditions" that make the category theory definition equivalent to ours.

```scala mdoc
def join[M[_], A](x: M[M[A]])(using m: Monad[M]): M[A] = m.bind(x, (y: M[A]) => y)
```

Here are some other common monads:

The Identity Monad
------------------

The _identity monad_ is the simplest monad which corresponds to ordinary function application. If we parameterize monadic code
with the identity monad, we get the behavior of the original non-monadic code.

```scala mdoc
type Id[X] = X
object IdentityMonad extends Monad[Id] {
  def bind[A, B](x: A, f: A => B): B = f(x)
  def unit[A](a: A): A = a
}
```


The Reader Monad
----------------

This is the _reader monad_, a.k.a. _environment monad_. It captures the essence of "environment-passing style".

The type parameter ``[A] =>> R => A`` may look a bit complicated, but
it is merely "currying" the function arrow type constructor.

The type constructor which is created here is ``M[A] = R => A``

```scala mdoc
trait ReaderMonad[R] extends Monad[[A] =>> R => A] {
  // pass the "environment" r into both computations
  override def bind[A, B](x: R => A, f: A => R => B): R => B = r => f(x(r))(r)
  override def unit[A](a: A): R => A = (_) => a
}
```

__Example__: Suppose that all functions in our API above depend on some kind of environment, say, the current configuration.
For simplicitly, let's assume  that the current configuration is just an ``Int``, hence all functions have a return type of
the form ``Int => A``:

```scala mdoc
def fRead(n: Int): Int => String = sys.error("not implemented")
def gRead(x: String): Int => Boolean = sys.error("not implemented")
def hRead(b: Boolean): Int => Int = sys.error("not implemented")
```

Our original code,

```scala
def clientCode = !g(f(27) + "z")
```

becomes:

```scala mdoc
def clientCodeRead(env: Int) = !(gRead(fRead(27)(env) + "z")(env))
```

In monadic form, the explicit handling of the environment disappears again:

```scala mdoc
def clientCode2Read(using m: ReaderMonad[Int]) =
  m.bind(fRead(27), (x: String) =>
    m.bind(gRead(x + "z"), (y: Boolean) =>
      m.unit(!y)))

/** this code does not work in older versions of Scala */
def clientCode2ReadFor(using m: ReaderMonad[Int]) =
  for {
    x <- fRead(27)
    y <- gRead(x + "z")
  } yield !y
```


The State Monad
---------------

The _state monad_, in which computations depend on a state ``S``
which is threaded through the computations, is defind as follows:

```scala mdoc
trait StateMonad[S] extends Monad[[A] =>> S => (A, S)] {
  override def bind[A, B](x: S => (A, S), f: A => S => (B, S)): S => (B, S) =
    // thread the state through the computations
    s => x(s) match { case (y, s2) => f(y)(s2) }
  override def unit[A](a: A): S => (A, S) = s => (a, s)
}
```

Example: Assume that our API maintains a state which (for simplicity) we assume to be a single integer. That is, it would look like this:

```scala mdoc
def fState(n: Int): Int => (String, Int) = sys.error("not implemented")
def gState(x: String): Int => (Boolean, Int) = sys.error("not implemented")
def hState(b: Boolean): Int => (Int, Int) = sys.error("not implemented")
```

The original code,

```scala
def clientCode = !g(f(27) + "z")
```

becomes:

```scala mdoc
def clientCodeState(s: Int) =
  fState(27)(s) match {
    case (x, s2) => gState(x + "z")(s2) match {
      case (y, s3) => (!y, s3) }}
```

In monadic style, however, the state handling disappears once more:

```scala mdoc
def clientCode2State(using m: StateMonad[Int]) =
  m.bind(fState(27), (x: String) =>
    m.bind(gState(x + "z"), (y: Boolean) =>
      m.unit(!y)))

/** This still does not work in Scala 3.
def clientCode2StateFor(using m: StateMonad[Int]) =
  for {
    x <- fState(27)
    y <- gState(x + "z")
  } yield !y
*/
```


The List Monad
--------------

In the _list monad_, computations produce lists of results. The ``bind`` operator combines all those results in a single list.

```scala mdoc
object ListMonad extends Monad[List] {
  // apply f to each element, concatenate the resulting lists
  override def bind[A, B](x: List[A], f: A => List[B]): List[B] = x.flatMap(f)
  override def unit[A](a: A) = List(a)
}
```

__Example__: Assume that our API functions return lists of results, and our client code must exercise the combination of all possible answers.

```scala mdoc
def fList(n: Int): List[String] = sys.error("not implemented")
def gList(x: String): List[Boolean] = sys.error("not implemented")
def hList(b: Boolean): List[Int] = sys.error("not implemented")
```

The original code,

```scala
def clientCode = !g(f(27) + "z")
```

becomes:

```scala mdoc
def clientCodeList =
  fList(27).map(x => gList(x + "z")).flatten.map(y => !y)
```

The monadic version of the client code stays the same, as expected:

```scala mdoc
def clientCode2List = {
  given Monad[List] = ListMonad
  for {
    x <- fList(27)
    y <- gList(x + "z")
  } yield !y
}
```


The Continuation Monad
----------------------

The last monad we are going to present is the continuation monad, which stands for computations that are continuations.

```scala mdoc
trait ContinuationMonad[R] extends Monad[[A] =>> (A => R) => R] {
  type Cont[X] = (X => R) => R

  override def bind[A, B](x: Cont[A], f: A => Cont[B]): Cont[B] =
    // construct continuation for x that calls f with the result of x
    k => x(a => f(a)(k))
  override def unit[A](a: A): Cont[A] = k => k(a)

  // callcc is like letcc; the difference is that letcc binds a name,
  // whereas callcc expects a function as argument.
  // That means that letcc(k, ...) is expressed as callcc(k => ...).
  def callcc[A, B](f: (A => Cont[B]) => Cont[A]): Cont[A] =
    k => f((a: A) => (_: B => R) => k(a))(k)
}
```

__Example__: Suppose our API was CPS-transformed:

```scala mdoc
def fCPS[R](n: Int): (String => R) => R = sys.error("not implemented")
def gCPS[R](x: String): (Boolean => R) => R = sys.error("not implemented")
def hCPS[R](b: Boolean): (Int => R) => R = sys.error("not implemented")
```

The original code,

```scala
def clientCode = !g(f(27) + "z")
```

becomes:

```scala mdoc
def clientCodeCPS[R]: (Boolean => R) => R =
  k => fCPS(27)((x: String) => gCPS(x + "z")((y: Boolean) =>  k(!y)))
```

The monadic version hides the CPS transformation in the operations of the monad.

```scala mdoc
def clientCode2CPS[R](using m: ContinuationMonad[R]) =
  m.bind(fCPS(27), (x: String) =>
    m.bind(gCPS(x + "z"), (y: Boolean) =>
      m.unit(!y)))

/** this still does not work in Scala 3
def clientCode2CPSFor[R](using m: ContinuationMonad[R]) =
  for {
    x <- fCPS(27)
    y <- gCPS(x + "z")
  } yield !y
*/
```

Let's implement ``1 + (2 + 3)`` in monadic style and implicitly CPS-transform using the continuation monad:

```scala mdoc
// unfortunately we can, again, not use for-comprehension syntax
def ex123[R](using m: ContinuationMonad[R])  = {
  m.bind(
    m.bind(m.unit(2), (two: Int) =>
      m.bind(m.unit(3), (three: Int) => m.unit(two + three))),
    (five: Int) => m.unit(1 + five))
}

def runEx123 = ex123(using new ContinuationMonad[Int]{})(x => x)
```

Let's implement the ``(+ 1 (let/cc k (+ 2 (k 3))))`` example using ``callcc``:

```scala mdoc
def excallcc[R](using m: ContinuationMonad[R])  = {
  m.bind(
    m.bind(m.unit(2), (two: Int) =>
      m.callcc[Int, Int](k => m.bind(k(3), (three: Int) => m.unit(two + three)))),
    (five: Int) => m.unit(1 + five))
}

def runExcallcc = excallcc(using new ContinuationMonad[Int]{})(x => x)
```

Remember how we had to CPS-transform the `map` function in the "allCosts" example when we talked about continuations?
Now we can use the monadic version `mapM` of `map` from above that works for any monad, including the continuation monad.


Monad Transformers
------------------

The purpose of monad transformers is to compose monads.
For instance, what if we want to have both the list monad and the option monad
at the same time? Such situations arise very often in practical code.

One solution is to have a monad transformer version of each monad, which is
parameterized with another monad. We show this for the Option monad case.

```scala mdoc
type OptionT[M[_]] = [A] =>> M[Option[A]]

class OptionTMonad[M[_]](val m: Monad[M]) extends Monad[OptionT[M]] {

  override def bind[A, B](x: M[Option[A]], f: A => M[Option[B]]): M[Option[B]] =
    m.bind(x, (z: Option[A]) => z match { case Some(y) => f(y)
                                          case None => m.unit(None) })

  override def unit[A](a: A) = m.unit(Some(a))

  def lift[A](x: M[A]): M[Option[A]] = m.bind(x, (a: A) => m.unit(Some(a)))
}
```

```scala mdoc:silent
val ListOptionM = new OptionTMonad(ListMonad)
```

```scala mdoc
// in this case, for-comprehension syntax doesn't work because it clashes with the
// built-in support for for-comprehensions for lists :-(
def example = {
  val m = ListOptionM
  m.bind(List(Some(3), Some(4)), (x: Int) => m.unit(x + 1))
}

def example2 = {
  val m = ListOptionM
  m.bind(List(Some(3), Some(4)), (x: Int) => m.lift(List(1, 2, 3, x)))
}

def example3 = {
  val m = ListOptionM
  m.bind(List(Some(3), None, Some(4)), (x: Int) => m.unit(x + 1))
}

def example4 = {
  val m = ListOptionM
  m.bind(List(Some(3), Some(4)), (x: Int) => if (x > 3) m.m.unit(None) else m.unit(x * 2))
}
```

Monad transformers are a standard way to compose monads, e.g., in Haskell and in Scala.
They have a number of well-known disadvantages. For instance, one needs additional transformer
versions of monads and the required lifting sometimes destroys modularity.
There are a number of alternative proposals to monad transformers, such as
["extensible effects"](https://hackage.haskell.org/package/extensible-effects).
