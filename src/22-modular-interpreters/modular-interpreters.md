# Modular Interpreters

The content of this chapter is available as a Scala file [here.](./modular-interpreters.scala)

```scala mdoc:invisible
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.reflectiveCalls
```


A Monad Library
================
 A Monad library in Scala. Monad libraries like
in ``scalaz`` and ``cats`` (for Scala) or the Haskell
standard library look similar.

```scala mdoc
trait Monad {
    type M[_] // this time we treat M as a type member and not as type parameter
              // because it leads to shorter type signatures
    def unit[A](a: A) : M[A]
    def bind[A,B](m: M[A], f: A => M[B]) : M[B]

    extension [A](m: M[A])
      def map[B](f: A => B): M[B] = bind(m, (x:A) => unit(f(x)))
      def flatMap[B](f: A => M[B]): M[B] = bind(m,f)
}
```

  We formulate concrete monads in the form of abstract interfaces first.
  The idea of those interfaces is that it should be possible to use
  the monad only in terms of that interface, without knowing anything
  about `M`.

  The advantage of this approach is that it enables us to
  compose monads. `M` changes in every composition of monads. For instance,
  when composing the list monad and the option monad, then
  ``M[X] = Option[List[X]]`` or ``M[X]=List[Option[X]]``.
  By keeping `M` abstract and using it only via the interfaces, "client code"
  does not need to depend on the particular composition of monads.

  The Reader (or Environment) monad captures computations that depend
  on an environment of type `R`.
  The ask function yields the current environment, the local function
  is used to transform the environment in a subcomputation a by an
  environment transformer `f`.

  ```scala mdoc
trait ReaderMonad extends Monad {
    type R
    def ask: M[R]
    def local[A](f: R => R, a: M[A]) : M[A]
}
```

The standard implementation of the Reader monad:

```scala mdoc
trait ReaderMonadImp extends ReaderMonad {
    type M[X] = R => X
    def unit[A](a: A) : M[A] = r => a
    def bind[A,B](m: M[A], f: A => M[B]) : M[B] = r => f(m(r))(r)
    def ask : M[R] = identity
    def local[A](f: R => R, a: M[A]) : M[A] = r => a(f(r))
}

/** An example of using the reader monad to propagate an environment
of type ``Int`` through a computation. **/

object ReaderExample extends ReaderMonadImp {
    type R = Int
    def example : M[Int] = for { x <- ask } yield (x+1)
    def example2 : M[Int] = local( r => 99, example)
}
```

A more useful example where we use the reader monad
to propagate a mapping from identifiers to boolean values
in an interpreter for boolean formulas.

```scala
object ReaderExample2 extends ReaderMonadImp {
```

```scala mdoc
    enum Exp:
      case Id(x: String)
      case And(l: Exp, r: Exp)
      case Or(l: Exp, r: Exp)
    import Exp._
    type R = Map[String,Boolean]
```
Note, that the signature of ``eval`` is identical to
  ``def eval(e: Exp) : Map[String,Boolean] => Boolean``

That is, we curry ``eval`` to make it applicable to the reader monad.


```scala mdoc
import Monad._
    def eval(e: Exp): M[Boolean] = e match {
        case Id(x) => for {env <- ask } yield env(x)
        case And(l,r) => for {
            x <- eval(l)
            y <- eval(r)
        } yield (x && y)
        // the implementation of the "And" case is semantically
        // equivalent to this code:
        //case And(l,r) => env => {
        //    val x = eval(l)(env)
        //    val y = eval(r)(env)
        //    x && y
        //}
        // However, the monadic code is more abstract (and hence
        // more reusable) because it is not coupled to the
        // concrete M
        case Or(l,r) => for {
            x <- eval(l)
            y <- eval(r)
        } yield (x || y)
    }
```

```scala
}
```

```scala mdoc
trait StateMonad extends Monad {
    type S
    def getState : M[S]
    def putState(s: S) : M[Unit]
}

trait StateMonadImp extends StateMonad {
    type M[A] = S => (A,S)
    def unit[A](a: A) : M[A] = (s: S) => (a,s)
    def bind[A,B](m: M[A], f: A => M[B]) : M[B] = (s: S) => {
        val (a,s2) = m(s)
        f(a)(s2)
    }
    def getState : M[S] = s => (s,s)
    def putState(s: S) : M[Unit] = _ => ((),s)
}
```

The continuation monad provides a method ``callcc``,
which reifies the current continuation ``k : A => M[B]``

```scala mdoc
trait ContinuationMonad extends Monad {
  def callcc[A,B](f : (A => M[B]) => M[A]) : M[A]
}
```
End of Monad Interfaces

Now we provide implementations of the monad interfaces.
The identity monad, which is the end of each transformer chain

```scala mdoc
trait IdentityMonad extends Monad {
  type M[A] = A
  def unit[A](a: A) : M[A] = a
  def bind[A,B](m: M[A], f: A => M[B]) = f(m)
}

object IdentityMonad extends IdentityMonad
```

We organize most other monads as monad _transformers_.

A monad transformer is parameterized with another monad.
The monads are organized in a chain.
Operations of "inner" monads must be lifted to top-level operations.

```scala mdoc
trait MonadTransformer extends Monad {
  val m : Monad
}
```

The Reader monad transformer. We provide some convenient
functions ``lift``, ``lift2`` etc. to lift functions from the inner monad.

Note that ``M[X] = R => m.M[X]`` instead of ``M[X] = R => X`` (as for
the non-transformer version of the reader monad).
The correct implementation of the interface methods follows from this type equation.

```scala mdoc
trait ReaderT extends MonadTransformer with ReaderMonad {
  type R
  override type M[X] = R => m.M[X]
  override def unit[A](a: A) : M[A] = r => m.unit(a)
  override def bind[A,B](x: M[A], f: A => M[B]) : M[B] = r => m.bind(x(r), (n:A) => f(n)(r))
  override def ask : M[R] = r => m.unit(r)
  override def local[A](f: R => R, a: M[A]) : M[A] = r => a(f(r))
  protected implicit def lift[A](x: m.M[A]) : M[A] = r => x
  protected implicit def lift2[A,B](x: A => m.M[B]) : A => M[B] = a => lift(x(a))
  protected implicit def lift3[A,B,C](x: (A => m.M[B]) => m.M[C]) : (A => M[B]) => M[C] = f => r => x( (a: A) => f(a)(r))
  protected implicit def lift4[A,B,C,D](x: ((A => m.M[B]) => m.M[C]) => m.M[D]) : ((A => M[B]) => M[C]) => M[D] = f => r => x( (a: A => m.M[B]) => f(lift2(a))(r))
}
```

The original Reader monad can be reconstructed by composing ``ReaderT`` with the ``identity`` monad.

```scala mdoc
trait ReaderMonadImpl extends ReaderT {
  val m: IdentityMonad = IdentityMonad
}
```

We do not need this because we have just synthesized it.
```
trait ReaderMonadImpl extends ReaderMonad {
  type M[X] = R => X
  def unit[A](a: A) : M[A] = r => a
  def bind[A,B](m: M[A], f: A => M[B]) : M[B] = r => f(m(r))(r)
  def ask : M[R] = identity
  def local[A](f: R => R, a: M[A]) : M[A] = (r) => a(f(r))  
}
```

The design of ``StateT`` is similar to that of ``ReaderT``

```scala mdoc
trait StateT extends MonadTransformer with StateMonad {
  type M[A] = S => m.M[(A,S)]
  override def unit[A](a: A) : M[A] = (s: S) => m.unit(a,s)
  override def bind[A,B](x: M[A], f: A => M[B]) : M[B] = (s: S) => {
     m.bind[(A,S),(B,S)](x(s), { case (a,s2) => f(a)(s2)})
  }
  override def getState : M[S] = s => m.unit((s,s))
  override def putState(s: S) : M[Unit] = _ => m.unit(((),s))
}
```

and again we can reconstruct the ordinary state monad.

```scala mdoc
trait StateMonadImpl extends StateT {
  val m: IdentityMonad = IdentityMonad
}
```

We do not need this because we have just synthesized it.

```
trait StateMonadImpl extends StateMonad {
  type M[A] = S => (A,S)
  def unit[A](a: A) : M[A] = (s: S) => (a,s)
  def bind[A,B](m: M[A], f: A => M[B]) : M[B] = (s: S) => {
     val (a,s2) = m(s)
     f(a)(s2)
  }
  def getState : M[S] = s => (s,s)
  def putState(s: S) : M[Unit] = _ => ((),s)
}
```


We could also synthesize ``ContinuationMonadImpl`` from a ``ContT``
just as we did for ``ReaderMonadImpl`` and ``StateMonadImpl``,
but for simplicity we only present the ordinary
``continuation`` monad here.

``scala mdoc
trait ContinuationMonadImpl extends ContinuationMonad {
  type T
  type M[A] = (A => T) => T
  override def unit[A](a: A) : M[A] = k => k(a)
  override def bind[A,B](m: M[A], f: A => M[B]): M[B] = k => m( a => f(a)(k))
  override def callcc[A,B](f : (A => M[B]) => M[A]) : M[A] = k => f(a => _ => k(a))(k)
}
```

The composition of the ``Reader`` monad and some ``continuation`` monad.

```scala mdoc
trait ReaderContinuationMonadForwarder extends ReaderT with ContinuationMonad {
  val m : ContinuationMonad
  override def callcc[A,B](f : (A => M[B]) => M[A]) : M[A] = (m.callcc[A,B] _)(f) // call to lift4 inserted automatically
}
```

The composition of the ``Reader`` monad and the ``continuation`` monad implementation.

```scala mdoc
trait ReaderContinuationMonadImpl extends ReaderContinuationMonadForwarder {
  type T
  val m: ContinuationMonadImpl { type T = ReaderContinuationMonadImpl.this.T } =
    new ContinuationMonadImpl { type T = ReaderContinuationMonadImpl.this.T }
}
```

Composition of ``reader`` monad with some ``state`` monad.

```scala mdoc
trait ReaderStateMonadForwarder extends ReaderT with StateMonad {
  val m: StateMonad { type S = ReaderStateMonadForwarder.this.S }
  override def getState : M[S] = m.getState
  override def putState(s: S) : M[Unit] = m.putState(s)
}
```

Composition of ``reader`` monad with ``StateMonadImpl``

```scala mdoc
trait ReaderStateMonadImpl extends ReaderStateMonadForwarder {
  val m: StateMonadImpl { type S = ReaderStateMonadImpl.this.S } =
    new StateMonadImpl { type S = ReaderStateMonadImpl.this.S }
}
```


Now we use the monad library to modularize the interpreters of
the various language variants we have seen so far.

```scala mdoc
trait Expressions extends Monad {
  abstract class Value
  abstract class Exp {
    def eval : M[Value]
  }
}

trait Numbers extends Expressions {
  case class NumV(n: Int) extends Value
}

trait Arithmetic extends Numbers {
  case class Num(n: Int) extends Exp {
    def eval = unit(NumV(n))
  }
  implicit def num2exp(n: Int): Exp = Num(n)

  case class Add(lhs: Exp, rhs: Exp) extends Exp {
    def eval = for {
                 l <- lhs.eval
                 r <- rhs.eval
               } yield (l,r) match {
                 case (NumV(v1), NumV(v2)) => NumV(v1+v2)
                 case _ => sys.error("can only add numbers")
               }
  }
}

trait If0 extends Numbers {
  case class If0(cond: Exp, thenExp: Exp, elseExp: Exp) extends Exp {
    def eval = for {
                c <- cond.eval
                res <- c match {
                  case NumV(0) => thenExp.eval
                  case _ => elseExp.eval
                }
              } yield res
  }
}

trait Functions extends Expressions with ReaderMonad {
  type Env = Map[String,Value]
  override type R = Env

  case class ClosureV(f: Fun, env: Env) extends Value
  case class Fun(param: String, body: Exp) extends Exp {
    def eval = for { env <- ask } yield ClosureV(this, env)
  }
  case class Ap(f: Exp, a: Exp) extends Exp {
    def eval = for {
                fv <- f.eval
                av <- a.eval
                res <- fv match {
                  case ClosureV(fun,cenv) => local( env => cenv + (fun.param -> av), fun.body.eval)
                }
       } yield res
  }
  case class Id(x: String) extends Exp {
    def eval = for {
               env <- ask
             } yield env(x)
  }
  implicit def id2exp(x: String): Exp = Id(x)
  def wth(x: String, xdef: Exp, body: Exp) : Exp = Ap(Fun(x,body),xdef)
}


trait Boxes extends Expressions with StateMonad  {
  override type S = Store
  type Store = Map[Address,Value]

  type Address = Int
  var _nextAddress = 0

  def nextAddress : Address = {
    _nextAddress += 1
    _nextAddress
  }

  case class AddressV(a: Address) extends Value

  case class NewBox(e: Exp) extends Exp {
    def eval = {
      val a = nextAddress
      for {
        v <- e.eval
        s <- getState
        _ <- putState(s + (a -> v))
      } yield AddressV(a)
    }
  }
  case class SetBox(b: Exp, e: Exp) extends Exp {
    def eval =
      for {
         box <- b.eval
         ev  <- e.eval
         s   <- getState
         _   <- putState(box match { case AddressV(a) => s.updated(a,ev) })
      } yield ev
  }


  case class OpenBox(b: Exp) extends Exp {
    def eval = for {
                 bv <- b.eval
                 s  <- getState
              } yield (bv match { case AddressV(a) => s(a) })
  }
  case class Seq(e1: Exp, e2: Exp) extends Exp {
    def eval = bind(e1.eval,(_:Value) => e2.eval)
  }

}

trait Letcc extends Expressions with ContinuationMonad with ReaderMonad{
  override type R = Map[String,Value]

  // We introduce a new application form CAp instead of using Ap because we cannot extend Ap
  case class CAp(f: Exp, a: Exp) extends Exp {
    override def eval : M[Value] =
      for {
         fv <- f.eval
         av <- a.eval
         res <- fv match { case ContV(f) => f(av) }
       } yield res
  }
  case class Letcc(param: String, body: Exp) extends Exp {
    override def eval : M[Value] = callcc[Value,Value](k => local( env => env + (param -> ContV(k)), body.eval))
  }
  case class ContV(f: Value => M[Value]) extends Value
}
```

Let's compose together some languages!

```scala mdoc
object AE extends Arithmetic with IdentityMonad {
  val aetest = Add(1,Add(2,3))
}
assert(AE.aetest.eval == AE.NumV(6))

object FAELang extends Functions with Arithmetic with ReaderMonadImpl {
  val faetest = Ap(Fun("x", Add("x", 1)), Add(2,3))
  assert(faetest.eval(Map.empty) == NumV(6))
}
object BCFAE extends Boxes with Arithmetic with Functions with If0 with ReaderStateMonadImpl {
  val test = wth("switch", NewBox(0),
                wth("toggle", Fun("dummy", If0(OpenBox("switch"),
                                          Seq(SetBox("switch", 1), 1),
                                          Seq(SetBox("switch", 0), 0))),
                             Add(Ap("toggle",42), Ap("toggle",42))))
}

assert(BCFAE.test.eval(Map.empty)(Map.empty)._1 == BCFAE.NumV(1))

object FAEwLetcc extends Arithmetic with Functions with If0 with Letcc with ReaderContinuationMonadImpl {
  override type T = Value
  val testprog = Add(1, Letcc("k", Add(2, CAp("k", 3))))
}

assert(FAEwLetcc.testprog.eval(Map.empty)(identity) == FAEwLetcc.NumV(4))
```
