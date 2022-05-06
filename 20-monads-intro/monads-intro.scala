/**
Monads
======
*/
import scala.language.higherKinds
import scala.language.reflectiveCalls

/**
We have seen various patterns of function composition:
- The environment passing style, in which an environment is passed down in recursive calls
- The store passing style, in which a store is threaded in and out of every computation
- The continuation passing style, in which every function call is a tail call.
Monads are way to abstract over such patterns of function composition.
Motivation
----------
Using monads, we can write code which can be parameterized to be in
one of the styles above (or many others).
Here is another common pattern of function composition. Suppose we have the following API of (nonsensical) functions:
*/
def f(n: Int) : String = "x"
def g(x: String) : Boolean = x == "x"
def h(b: Boolean) : Int = if (b) 27 else sys.error("error")

def clientCode = h(!g(f(27)+"z"))

/**
Now suppose that these functions can possibly fail (say, because they involve remote communication). A common way to deal with such
failures is to use the Option datatype:
*/

def fOp(n: Int) : Option[String] = if (n < 100) Some("x") else None
def gOp(x: String) : Option[Boolean] = Some(x == "x")
def hOp(b: Boolean) : Option[Int] = if (b) Some(27) else None

/**
However, now the clientCode must be changed rather dramatically:
*/

def clientCodeOp =
  fOp(27) match {
    case Some(x) => gOp(x+"z") match {
            case Some(y) => hOp(!y)
            case None => None
      }
    case None => None
  }

/**
We see a kind of pattern in this code. We have a value of type ``Option[A]``, but the next function we need to call requires an A and
produces an ``Option[B]``. If the ``Option[A]`` value is ``None``, then the whole computation produces ``None``. If it is ``Some(x)``
instead, we pass ``x`` to the function.
We can capture this pattern in the form of a function:
*/

def bindOption[A,B](a: Option[A], f: A => Option[B]) : Option[B] = a match {
  case Some(x) => f(x)
  case None => None
}

/**
Using bindOption, we can rewrite the code above as follows:
*/

def clientCodeOpBind =
  bindOption(fOp(27), (x:String) =>
    bindOption(gOp(x+"z"), (y:Boolean) =>
      hOp(!y)))

/**
Now suppose that our original client code was not ``h(!g(f(27)+"z"))``
but instead ``!g(f(27)+"z")``. How can we express this with bind? This
thing does not type check:
    def clientCode =
      f(27) bind ((x: String) =>
      g(x+"z") bind  ((y: Boolean) =>
      !y))
One way to fix the situation is to insert a call to ``Some``, like so:
*/

def clientCode2Op =
  bindOption(fOp(27), ((x: String) =>
  bindOption(gOp(x+"z"), ((y: Boolean) =>
  Some(!y)))))

/**
While this works, it is incompatible with our original goal of abstracting over the function composition pattern, because the
Some constructor exposes what kind of pattern we are currently dealing with. Hence let's abstract over it by adding a second
function "unit" to our function composition interface:
*/

def unit[A](x: A) : Option[A] = Some(x)

def clientCode2OpUnit =
  bindOption(fOp(27), ((x: String) =>
  bindOption(gOp(x+"z"), ((y: Boolean) =>
  unit(!y)))))

/**
This looks better, but the types of unit and bind still reveal that we are dealing with the "Option" function composition pattern.
Let's abstract over the Option type constructor by turning the type constructor into a parameter. The resulting triple
(type constructor, unit function, bind function) is called a _monad_. Certain conditions (the "monad laws") on unit and bind also
need to hold to make it a true monad, but we'll defer a discussion of these conditions until later.
The Monad Interface
-------------------

So here it is: The Monad interface.
*/

trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def bind[A,B](m: M[A], f: A => M[B]): M[B]
  // The "monad laws":
  // 1) "unit" acts as a kind of neutral element of "bind", that is:
  //    1a) bind(unit(x),f) == f(x) and
  //    1b) bind(x, y => unit(y)) == x
  // 2) Bind enjoys an associative property
  //     bind(bind(x,f),g) == bind(x, y => bind(f(y),g))
}

/**
Using this interface, we can now make clientCode depend only on this interface, but no longer on the Option type:
*/

def clientCode2Op(m: Monad[Option]) =
  m.bind(fOp(27), (x: String) =>
  m.bind(gOp(x+"z"), (y: Boolean) =>
  m.unit(!y)))

/**
If the API is parametric in the monad, we can make the client code fully parametric, too. We model the monad object as an
implicit parameter to save the work of passing on the monad in every call.
*/

def fM[M[_]](n: Int)(using m: Monad[M]) : M[String] = sys.error("not implemented")
def gM[M[_]](x: String)(using m: Monad[M]) : M[Boolean] = sys.error("not implemented")
def hM[M[_]](b: Boolean)(using m: Monad[M]) : M[Int] = sys.error("not implemented")

def clientCode2[M[_]](using m: Monad[M]) =
  m.bind(fM(27), (x: String) =>
  m.bind(gM(x+"z"), (y: Boolean) =>
  m.unit(!y)))

/**
For-Comprehension Syntax
------------------------
All these nested calls to bind can make the code hard to read. Luckily, there is a notation called "monad comprehension" to make
monadic code look simpler. Monad comprehensions are directly supported in Haskell and some other languages. In Scala, we can
piggy-back on the "For comprehension" syntax instead.
A "for comprehension"  is usually used for lists and other collections. For instance:
    val l = List(List(1,2), List(3,4))
    assert( (for { x <- l; y <- x } yield y+1) == List(2,3,4,5))
The Scala compiler desugars the for-comprehension above into calls of the standard map and flatMap functions. That is, the above
for comprehension is equivalent to:
    assert(l.flatMap(x => x.map(y => y+1)) == List(2,3,4,5))
We will make use of for-comprehension syntax by supporting both ``flatMap`` (which is like ``bind``) and ``map`` (which is like ``fmap``).
We support these functions by an implicit conversion to an object that supports these functions as follows:
*/

extension [A, M[_]](m: M[A])(using mm: Monad[M])
  def map[B](f: A => B): M[B] = mm.bind(m, (x: A) => mm.unit(f(x)))
  def flatMap[B](f: A => M[B]): M[B] = mm.bind(m, f)

/**
Using the new support for for-comprehension syntax, we can rewrite our client code as follows: Given the API from above,

def fOp(n: Int) : Option[String] = if (n < 100) Some("x") else None
def gOp(x: String) : Option[Boolean] = Some(x == "x")
def hOp(b: Boolean) : Option[Int] = if (b) Some(27) else None


we can now rewrite this:

def clientCode2Op(m: Monad[Option]) =
  m.bind(fOp(27), (x: String) =>
  m.bind(gOp(x+"z"), (y: Boolean) =>
  m.unit(!y)))

to this:
*/

def clientCode2OpFor(using m: Monad[Option]) =
  for {
    x <- fOp(27)
    y <- gOp(x+"z")
  } yield !y


/**
The Option Monad
----------------
Let's look at some concrete monads now. We have of course already seen one particular Monad: The Option monad. This monad is also
sometimes called the Maybe monad.
*/

object OptionMonad extends Monad[Option] {
  override def bind[A,B](a: Option[A], f: A => Option[B]) : Option[B] =
    a match {
      case Some(x) => f(x)
      case None => None
    }
  override def unit[A](a: A) = Some(a)
}

/**
We can now parameterize clientCode with OptionMonad.
*/

def v : Option[Boolean] = clientCode2Op(OptionMonad)

/**
There are many other sensible monads. Before we discuss those, let us discuss whether there are useful functions that are generic
enough to be useful for many different monads. Here are some of these functions:
*/

// fmap turns every function between A and B into a function between M[A] and M[B]
def fmap[M[_],A,B](f: A => B)(using m: Monad[M]): M[A] => M[B] = a => m.bind(a,(x:A) => m.unit(f(x)))
// In fancy category theory terms, we can say that every monad is a functor.

// sequence composes a list of monadic values into a single monadic value which is a list.
def sequence[M[_],A](l: List[M[A]])(using m: Monad[M]) : M[List[A]] = l match {
  case x :: xs => m.bind(x, (y: A) =>
    m.bind(sequence(xs), (ys : List[A]) =>
      m.unit(y :: ys)))
  case Nil => m.unit(List.empty)
}

// mapM composes sequence and the standard map function.
def mapM[M[_],A,B](f : A => M[B], l: List[A])(using m: Monad[M]) : M[List[B]] =
  sequence(l.map(f))

// join is another useful function to unwrap a layer of monads
// in category theory, monads are defined via unit (denoted by the greek letter "eta")
// and join ("mu") instead of unit and bind. There are additional "naturality" and
// "coherence conditions" that make the category theory definition equivalent to ours.
def join[M[_],A](x : M[M[A]])(using m: Monad[M]) : M[A] = m.bind(x, (y : M[A]) => y)

/** Here are some other common monads:
The Identity Monad
------------------

The _identity monad_ is the simplest monad which corresponds to ordinary function application. If we parameterize monadic code
with the Identity monad, we get the behavior of the original non-monadic code.
*/

type Id[X] = X
object IdentityMonad extends Monad[Id] {
  def bind[A,B](x: A, f: A => B) : B = f(x)
  def unit[A](a: A) : A = a
}



/**
The Reader Monad
----------------
This is the _reader monad_, a.k.a. _environment monad_. It captures the essence of "environment passing style".
*/

// The type parameter ({type M[A] = R => A})#M looks complicated, but
// it is merely "currying" the function arrow type constructor.
// The type constructor which is created here is M[A] = R => A
trait ReaderMonad[R] extends Monad[[A] =>> R => A] {
  override def bind[A,B](x: R => A, f: A => R => B) : R => B = r => f(x(r))(r) // pass the "environment" r into both computations
  override def unit[A](a: A) : R => A = (_) => a
}

/**
Example: Suppose that all functions in our API above depend on some kind of environment, say, the current configuration.
the form ``Int => A```:
