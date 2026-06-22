# Modular Interpreters

The content of this chapter is available as a Scala file [here.](./modular-interpreters.scala)

```scala mdoc:invisible
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.reflectiveCalls
```

# A Monad Library

We define a Monad library in Scala. Monad libraries like
in scalaz and cats (for Scala) or the Haskell standard library look similar.

```scala mdoc
trait Monad {
  type M[_] // this time we treat M as a type member and not as type parameter
            // because it leads to shorter type signatures
  def unit[A](a: A): M[A]
  def bind[A, B](m: M[A], f: A => M[B]): M[B]

  extension [A](m: M[A])
    def map[B](f: A => B): M[B] = bind(m, (x: A) => unit(f(x)))
    def flatMap[B](f: A => M[B]): M[B] = bind(m, f)
}
```

We formulate concrete monads in the form of abstract interfaces first.
The idea of those interfaces is that it should be possible to use
the monad only in terms of that interface, without knowing anything
about `M`. The advantage of this approach is that it enables us to
compose monads. `M` changes in every composition of monads. For instance,
when composing the list monad and the option monad, then
``M[X] = Option[List[X]]`` or ``M[X] = List[Option[X]]``.

By keeping `M` abstract and using it only via the interfaces, "client code"
does not need to depend on the particular composition of monads.

### Reader Monad

The Reader (or Environment) monad captures computations that depend
on an environment of type `R`.

The `ask` function yields the current environment, the `local` function
is used to transform the environment in a subcomputation a by an
environment transformer `f`.

```scala mdoc
trait ReaderMonad extends Monad {
  type R
  def ask: M[R]
  def local[A](f: R => R, a: M[A]): M[A]
}
```

The standard implementation of the Reader monad:

```scala mdoc
trait ReaderMonadImp extends ReaderMonad {
  type M[X] = R => X
  def unit[A](a: A): M[A] = r => a
  def bind[A, B](m: M[A], f: A => M[B]): M[B] = r => f(m(r))(r)
  def ask: M[R] = identity
  def local[A](f: R => R, a: M[A]): M[A] = r => a(f(r))
}
```

An example of using the Reader monad to propagate an environment
of type `Int` through a computation.

```scala mdoc
object ReaderExample extends ReaderMonadImp {
  type R = Int
  def example: M[Int] = for { x <- ask } yield (x + 1)
  def example2: M[Int] = local(r => 99, example)
}
```

A more useful example where we use the Reader monad
to propagate a mapping from identifiers to boolean values
in an interpreter for boolean formulas. Note that the signature of
`eval` is identical to `def eval(e: Exp): Map[String, Boolean] => Boolean`,
that is, we curry `eval` to make it applicable to the Reader monad.

```scala mdoc
object ReaderExample2 extends ReaderMonadImp {
  enum Exp:
    case Id(x: String)
    case And(l: Exp, r: Exp)
    case Or(l: Exp, r: Exp)
  import Exp._
  type R = Map[String, Boolean]

  def eval(e: Exp): M[Boolean] = e match {
    case Id(x) => for {env <- ask } yield env(x)
    case And(l, r) => for {
      x <- eval(l)
      y <- eval(r)
    } yield (x && y)
    case Or(l, r) => for {
      x <- eval(l)
      y <- eval(r)
    } yield (x || y)
  }
}
```

The implementation of the `And` case is semantically equivalent to this code:

```scala
case And(l, r) => env => {
  val x = eval(l)(env)
  val y = eval(r)(env)
  x && y
}
```

However, the monadic code is more abstract (and hence
more reusable) because it is not coupled to the concrete `M`.

### State Monad

This is the interface and standard implementation for the State monad:

```scala mdoc
trait StateMonad extends Monad {
  type S
  def getState: M[S]
  def putState(s: S): M[Unit]
}

trait StateMonadImp extends StateMonad {
  type M[A] = S => (A, S)
  def unit[A](a: A): M[A] = (s: S) => (a, s)
  def bind[A, B](m: M[A], f: A => M[B]): M[B] = (s: S) => {
    val (a, s2) = m(s)
    f(a)(s2)
  }
  def getState: M[S] = s => (s, s)
  def putState(s: S): M[Unit] = _ => ((), s)
}
```

### Continuation Monad

And here is the interface for the Continuation monad. The Continuation monad provides
a method ``callcc``, which reifies the current Continuation ``k: A => M[B]``.

```scala mdoc
trait ContinuationMonad extends Monad {
  def callcc[A, B](f: (A => M[B]) => M[A]): M[A]
}
```



## Implementaions

Now we provide implementations of the monad interfaces.

The identity monad, which is the end of each transformer chain reads:

```scala mdoc
trait IdentityMonad extends Monad {
  type M[A] = A
  def unit[A](a: A): M[A] = a
  def bind[A, B](m: M[A], f: A => M[B]) = f(m)
}

object IdentityMonad extends IdentityMonad
```

We organize most other monads as monad _transformers_.
A monad transformer is parameterized with another monad.
The monads are organized in a chain. Operations of "inner" monads must be lifted to top-level operations.

```scala mdoc
trait MonadTransformer extends Monad {
  val m: Monad
}
```

### Reader Monad Transformer

The Reader monad transformer. We provide some convenient
functions ``lift``, ``lift2`` etc. to lift functions from the inner monad.
Note that ``M[X] = R => m.M[X]`` instead of ``M[X] = R => X`` (as for
the non-transformer version of the Reader monad).
The correct implementation of the interface methods follows from
this type equation.

```scala mdoc
trait ReaderT extends MonadTransformer with ReaderMonad {
  type R
  override type M[X] = R => m.M[X]
  override def unit[A](a: A): M[A] = r => m.unit(a)
  override def bind[A, B](x: M[A], f: A => M[B]): M[B] =
    r => m.bind(x(r), (n: A) => f(n)(r))
  override def ask: M[R] = r => m.unit(r)
  override def local[A](f: R => R, a: M[A]): M[A] = r => a(f(r))
  protected implicit def lift[A](x: m.M[A]): M[A] = r => x
  protected implicit def lift2[A, B](x: A => m.M[B]): A => M[B] = a => lift(x(a))
  protected implicit def lift3[A, B, C](x: (A => m.M[B]) => m.M[C]): (A => M[B]) => M[C] =
    f => r => x((a: A) => f(a)(r))
  protected implicit def lift4[A, B, C, D](x: ((A => m.M[B]) => m.M[C]) => m.M[D]): ((A => M[B]) => M[C]) => M[D] =
    f => r => x((a: A => m.M[B]) => f(lift2(a))(r))
}

// The original Reader monad can be reconstructed by composing ReaderT with the identity monad.

trait ReaderMonadImpl extends ReaderT {
  val m: IdentityMonad = IdentityMonad
}
```

We do not need this because we have just synthesized it:

```scala
trait ReaderMonadImpl extends ReaderMonad {
  type M[X] = R => X
  def unit[A](a: A): M[A] = r => a
  def bind[A, B](m: M[A], f: A => M[B]): M[B] = r => f(m(r))(r)
  def ask: M[R] = identity
  def local[A](f: R => R, a: M[A]): M[A] = (r) => a(f(r))
}
```

### State Monad Transformer

The design of ``StateT`` is similar to that of ``ReaderT``:

```scala mdoc
trait StateT extends MonadTransformer with StateMonad {
  type M[A] = S => m.M[(A, S)]
  override def unit[A](a: A): M[A] = (s: S) => m.unit(a, s)
  override def bind[A, B](x: M[A], f: A => M[B]): M[B] = (s: S) => {
     m.bind[(A, S), (B, S)](x(s), { case (a, s2) => f(a)(s2)})
  }
  override def getState: M[S] = s => m.unit((s, s))
  override def putState(s: S): M[Unit] = _ => m.unit(((), s))
}

// and again we can reconstruct the ordinary State monad.

trait StateMonadImpl extends StateT {
  val m: IdentityMonad = IdentityMonad
}
```

We do not need this because we have just synthesized it:

```scala
trait StateMonadImpl extends StateMonad {
  type M[A] = S => (A, S)
  def unit[A](a: A): M[A] = (s: S) => (a, s)
  def bind[A, B](m: M[A], f: A => M[B]): M[B] = (s: S) => {
     val (a, s2) = m(s)
     f(a)(s2)
  }
  def getState: M[S] = s => (s, s)
  def putState(s: S): M[Unit] = _ => ((), s)
}
```

### Continuation Monad

We could also synthesize ``ContinuationMonadImpl`` from a ``ContT``
just as we did for ``ReaderMonadImpl`` and ``StateMonadImpl``,
but for simplicity we only present the ordinary Continuation monad here.

```scala mdoc
trait ContinuationMonadImpl extends ContinuationMonad {
  type T
  type M[A] = (A => T) => T
  override def unit[A](a: A): M[A] = k => k(a)
  override def bind[A, B](m: M[A], f: A => M[B]): M[B] = k => m(a => f(a)(k))
  override def callcc[A, B](f: (A => M[B]) => M[A]): M[A] = k => f(a => _ => k(a))(k)
}
```

The whole thing turns on reading `M[A] = (A => T) => T` correctly: a computation that *will eventually produce an `A`* is represented as a function that, given the continuation `A => T` describing the rest of the program, produces the final answer of type `T`. This is just CPS reified as a monad, with a fixed global answer type `T`. `unit(a)` hands `a` straight to whatever continuation is waiting, and `bind(m, f)` runs `m` under a continuation that takes `m`'s result `a`, forms the next computation `f(a)`, and runs *that* under the original `k`.

Now `callcc`. Its signature is

```scala
def callcc[A, B](f: (A => M[B]) => M[A]): M[A]
```

so `f` is a block that gets handed a first-class continuation of type `A => M[B]` and produces a computation `M[A]`. The implementation, with everything inlined:

```scala
k => f(a => _ => k(a))(k)
```

Read it outside-in. Since the result is an `M[A]`, it begins `k => …`, and that `k : A => T` is precisely *the continuation in force at the point where `callcc` was invoked* — the "rest of the program" relative to the `callcc` expression. This is the continuation we are capturing.

The captured continuation is reified as `g = a => _ => k(a)`. Check its type: given `a : A`, the body `_ => k(a)` has type `(B => T) => T`, which is exactly `M[B]`, so `g : A => M[B]` as required. The crucial part is the underscore. When you later invoke `g(a)`, you get back a computation that **ignores its own continuation** (the `_ : B => T`) and instead applies the captured `k` to `a`. That discarding of the local continuation is what makes invoking `g` a non-local jump rather than an ordinary return.

Finally, `f(g)(k)`: we give `f` the reified continuation and then run the resulting `M[A]` under the very same `k`. So there are two paths:

- If `f` never calls `g`, then `f(g)(k)` just runs `f`'s body normally and its `A`-result flows into `k` exactly as it would have without `callcc`. The capture was inert.
- If `f` *does* call `g(a)` somewhere inside, then at that moment the locally-pending continuation is thrown away and control resumes at `k(a)` — that is, `a` becomes the value of the *whole* `callcc` expression, and evaluation proceeds as if `callcc` had simply returned `a`.

## Compositions

Let's compose some monads.

The composition of the Reader monad and some Continuation monad.

```scala mdoc
trait ReaderContinuationMonadForwarder extends ReaderT with ContinuationMonad {
  val m: ContinuationMonad
  // call to lift4 inserted automatically
  override def callcc[A, B](f: (A => M[B]) => M[A]): M[A] = (m.callcc[A, B] _)(f)
}
```

For the implementation, we use the Continuation-monad implementation.

```scala mdoc
trait ReaderContinuationMonadImpl extends ReaderContinuationMonadForwarder {
  type T
  val m: ContinuationMonadImpl { type T = ReaderContinuationMonadImpl.this.T } =
    new ContinuationMonadImpl { type T = ReaderContinuationMonadImpl.this.T }
}
```

Composition of the Reader monad with some State monad.

```scala mdoc
trait ReaderStateMonadForwarder extends ReaderT with StateMonad {
  val m: StateMonad { type S = ReaderStateMonadForwarder.this.S }
  override def getState: M[S] = m.getState
  override def putState(s: S): M[Unit] = m.putState(s)
}
```

And the implementation with StateMonadImpl.

```scala mdoc
trait ReaderStateMonadImpl extends ReaderStateMonadForwarder {
  val m: StateMonadImpl { type S = ReaderStateMonadImpl.this.S } =
    new StateMonadImpl { type S = ReaderStateMonadImpl.this.S }
}
```


## A Modular Interpreter

Now we use the monad library to modularize the interpreters of
the various language variants we have seen so far.

```scala mdoc
trait Expressions extends Monad {
  abstract class Value
  abstract class Exp {
    def eval: M[Value]
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
               } yield (l, r) match {
                 case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
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
  type Env = Map[String, Value]
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
                   case ClosureV(fun, cenv) =>
                     local(env => cenv + (fun.param -> av), fun.body.eval)
                 }
               } yield res
  }
  case class Id(x: String) extends Exp {
    def eval = for {
                 env <- ask
               } yield env(x)
  }
  implicit def id2exp(x: String): Exp = Id(x)
  def wth(x: String, xdef: Exp, body: Exp): Exp = Ap(Fun(x, body), xdef)
}


trait Boxes extends Expressions with StateMonad  {
  type Store = Map[Address, Value]
  override type S = Store

  type Address = Int
  var _nextAddress = 0

  def nextAddress: Address = {
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
         _   <- putState(box match { case AddressV(a) => s.updated(a, ev) })
      } yield ev
  }
  case class OpenBox(b: Exp) extends Exp {
    def eval = for {
                 bv <- b.eval
                 s  <- getState
               } yield (bv match { case AddressV(a) => s(a) })
  }
  case class Seq(e1: Exp, e2: Exp) extends Exp {
    def eval = bind(e1.eval, (_: Value) => e2.eval)
  }

}

trait Letcc extends Expressions with ContinuationMonad with ReaderMonad {
  override type R = Map[String, Value]

  // We introduce a new application form CAp instead of using Ap because we cannot extend Ap
  case class CAp(f: Exp, a: Exp) extends Exp {
    override def eval: M[Value] =
      for {
         fv <- f.eval
         av <- a.eval
         res <- fv match { case ContV(f) => f(av) }
       } yield res
  }
  case class Letcc(param: String, body: Exp) extends Exp {
    override def eval: M[Value] =
      callcc[Value, Value](k => local(env => env + (param -> ContV(k)), body.eval))
  }
  case class ContV(f: Value => M[Value]) extends Value
}
```

Let's compose together some languages!

```scala mdoc
object AE extends Arithmetic with IdentityMonad {
  val aetest = Add(1, Add(2, 3))
}
assert(AE.aetest.eval == AE.NumV(6))

object FAELang extends Functions with Arithmetic with ReaderMonadImpl {
  val faetest = Ap(Fun("x", Add("x", 1)), Add(2, 3))
  assert(faetest.eval(Map.empty) == NumV(6))
}
object BCFAE extends Boxes with Arithmetic with Functions with If0 with ReaderStateMonadImpl {
  val test = wth("switch", NewBox(0),
                wth("toggle", Fun("dummy", If0(OpenBox("switch"),
                                          Seq(SetBox("switch", 1), 1),
                                          Seq(SetBox("switch", 0), 0))),
                    Add(Ap("toggle", 42), Ap("toggle", 42))))
}

assert(BCFAE.test.eval(Map.empty)(Map.empty)._1 == BCFAE.NumV(1))

object FAEwLetcc extends Arithmetic with Functions with If0 with Letcc with ReaderContinuationMonadImpl {
  override type T = Value
  val testprog = Add(1, Letcc("k", Add(2, CAp("k", 3))))
}

assert(FAEwLetcc.testprog.eval(Map.empty)(identity) == FAEwLetcc.NumV(4))
```



# When Does a Monad Have a Transformer?

We have just seen that the `Option` monad has a transformer version `OptionT`, parameterized
by an arbitrary inner monad `M`. A natural question is whether *every* monad can be turned
into a transformer in this way. The answer is **no**, and it is worth understanding precisely
where the construction comes from and where it breaks down.

The short version: a monad has a transformer exactly when its type is a *transparent recipe* —
a type expression in which we can point to the place where the result `A` is produced. The
transformer is then built by wrapping that place in the inner monad `M[_]` and sequencing with
the inner monad's own `bind`. If we cannot see where the result sits — because the monad is an
opaque primitive — there is no place to thread `M` through, and no transformer exists.

### The recipe

Look again at `OptionTMonad`. The plain `Option` monad uses the type constructor `M[A] = Option[A]`,
and `bind` pattern-matches on `Some`/`None`. To get the transformer we changed the type constructor
to `M[Option[A]]` and let the *inner* monad's `bind` do the sequencing:

```scala
type OptionT[M[_]] = [A] =>> M[Option[A]]

class OptionTMonad[M[_]](val m: Monad[M]) extends Monad[OptionT[M]] {
  override def unit[A](a: A): M[Option[A]] = m.unit(Some(a))
  override def bind[A, B](x: M[Option[A]], f: A => M[Option[B]]): M[Option[B]] =
    m.bind(x, (z: Option[A]) => z match {
      case Some(y) => f(y)
      case None    => m.unit(None)
    })
  def lift[A](x: M[A]): M[Option[A]] = m.bind(x, (a: A) => m.unit(Some(a)))
}
```

The same recipe works for the State monad. Recall that the plain State monad uses
`M[A] = S => (A, S)`. The result `A` is produced inside the pair returned by the function, so we
wrap *that* in the inner monad: `M[A] = S => m.M[(A, S)]`. Everywhere the plain `bind` would
hand the pair `(a, s2)` to the continuation, the transformer threads it through `m.bind` instead:

```scala
type StateT[S, M[_]] = [A] =>> S => M[(A, S)]

class StateTMonad[S, M[_]](val m: Monad[M]) extends Monad[StateT[S, M]] {
  override def unit[A](a: A): S => M[(A, S)] = s => m.unit((a, s))
  override def bind[A, B](x: S => M[(A, S)], f: A => S => M[(B, S)]): S => M[(B, S)] =
    s => m.bind(x(s), { case (a, s2) => f(a)(s2) })
  def lift[A](x: M[A]): S => M[(A, S)] = s => m.bind(x, (a: A) => m.unit((a, s)))
}
```

This is exactly the `StateT` we synthesize in the modular-interpreters chapter, where composing it
with the identity monad recovers the ordinary State monad. The point to take away is that the
construction is *mechanical*: it is driven entirely by the shape of the type. Because we can see
that the result `A` appears inside `S => (A, S)`, we know where to insert `m.M[...]` and where to
call `m.bind`. The `lift` function, which embeds a computation of the inner monad into the
transformed monad, falls out of the same shape.

### Transformer is not the same as composition

It is tempting to think a transformer just stacks one monad on top of another. Sometimes it
looks that way: `OptionT[M][A]` really is `M[Option[A]]`, the inner monad wrapped around the outer
functor. But `StateT` is **not** a composition. Neither `M[S => (A, S)]` nor `(S => (A, S))`
composed with `M` is the right type; the inner monad sits *inside*, wrapped only around the result
of the state function. Composing monads naively (as in `Option[List[A]]` versus `List[Option[A]]`)
is a strictly weaker idea. A transformer threads the inner monad through the *specific structure*
of the outer one, which is why it needs that structure to be visible.

### Monads that have a transformer

All the monads from the previous chapters are transparent type formers, so each admits a
transformer, built by the recipe above:

- **Option / Maybe** — `M[Option[A]]`; `bind` branches on `Some`/`None`, which we can see.
- **Reader** — `R => A` becomes `R => M[A]`; we wrap the result of the environment function.
- **Writer** — `(A, W)` becomes `M[(A, W)]`; we wrap the value-and-output pair.
- **State** — `S => (A, S)` becomes `S => M[(A, S)]`, as shown above.
- **List / nondeterminism** — `List[A]` becomes `M[List[A]]` (with the caveat below).

The continuation monad `Cont[A] = (A => R) => R` is a more interesting case. It also has a
transformer,

```scala
type ContT[R, M[_]] = [A] =>> (A => M[R]) => M[R]
```

but notice that the inner monad barely participates: `unit` and `bind` can be written almost
exactly as in the plain continuation monad, with `M[R]` simply standing in for `R`. This is a hint
that `ContT` does not behave like the others — it sits awkwardly in a transformer stack, and the
higher-order operations (`callcc`, and `local` for Reader) are the ones that are hard to lift
through it. So a transformer can exist without being well-behaved; see the caveat at the end.

### The monad that does not: IO

Contrast all of this with the IO monad. In the IO chapter we deliberately presented it as an
**abstract interface**, with the type constructor kept abstract:

```scala
trait IOMonad {
  type IO[_]                              // abstract: we may not look inside
  def unit[A](a: A): IO[A]
  def bind[A, B](m: IO[A], f: A => IO[B]): IO[B]
  def printString(s: String): IO[Unit]
  def inputString: IO[String]
  def performIO[A](action: IO[A]): A
}
```

That abstraction is the whole point, not an accident of presentation. The real IO monad is a
primitive provided by the runtime: `printString` actually prints, `inputString` actually reads, and
`bind` sequences these *real effects on the real world*. There is no data structure with a visible
"result position" that we could wrap in `M[_]`, and no seam between two real-world actions where we
could run an arbitrary inner monad's effects. Since the recipe needs a place to insert `m.bind` and
IO offers none, an `IOT[M]` simply cannot be written.

The toy implementation in the IO chapter makes this sharp. There we modelled IO as
`type IO[A] = World => (A, World)` — which is *structurally just the State monad* over a `World`.
If that were genuinely the IO monad, then `IOT` would be nothing but `StateT[World, M]` and there
would be no difficulty at all. But the genuine effect is the `println` and `readLine` performed as a
side effect of evaluation, *not* the `World` value being threaded; the `World` string is only a
fiction to illustrate the interface. The real effect is opaque, so it cannot be threaded through an
inner monad. Keeping `type IO[_]` abstract is precisely the formal statement that we are not allowed
to look inside.

The consequence mirrors a fact we already use in the modular interpreters. There, the identity
monad sits at the *end* of every transformer chain (composing `StateT` or `ReaderT` with it recovers
the plain monad). The IO monad sits at the opposite end: it is the *base* of a chain. We build
`ReaderT`, `StateT`, `OptionT` and so on *on top of* IO, never an `IOT` on top of something else, and
we reach the IO operations by lifting them up through the stack (our `lift`/`lift2` functions; in
Haskell this is the role of `liftIO`). More generally, **any monad given only as an opaque primitive,
with no inspectable structure, has no transformer for the same reason.**

### Caveat: existing is not the same as well-behaved

Even when a transformer can be written, it need not satisfy the monad laws or compose cleanly. The
list monad is the classic warning: the obvious `ListT[M][A] = M[List[A]]` fails to be a lawful monad
for many inner monads `M` (this is the well-known "`ListT` done wrong"), and a correct version is
considerably more delicate. And as noted above, `ContT` exists but makes lifting of higher-order
operations awkward. This is the same modularity problem mentioned earlier: the lifting that
transformers require sometimes breaks down, which is one of the reasons alternatives such as
extensible effects were proposed.

### A Transformer That Breaks the Monad Laws

We said that a transformer can exist without being well-behaved. The list monad is the standard
warning, so let us see concretely how it goes wrong.

Following the recipe, the obvious list transformer wraps the inner monad around the list:

```scala
type ListT[M[_]] = [A] =>> M[List[A]]

class ListTMonad[M[_]](val m: Monad[M]) extends Monad[ListT[M]] {
  given Monad[M] = m
  override def unit[A](a: A): M[List[A]] = m.unit(List(a))
  override def bind[A, B](x: M[List[A]], f: A => M[List[B]]): M[List[B]] =
    m.bind(x, (xs: List[A]) =>
      m.bind(mapM(f, xs), (xss: List[List[B]]) =>      // run f on every element
        m.unit(xss.flatten)))                          // then concatenate
}
```

This type-checks and looks reasonable: run the inner computation to get a list `xs`, apply `f` to
every element (each application is itself an inner computation), and concatenate the results. The
problem is that it quietly violates the associativity law

```
bind(bind(x, f), g) == bind(x, y => bind(f(y), g))
```

whenever the inner monad's effects are order-sensitive.

#### The counterexample

Take the inner monad to be the `IO` monad from the previous chapter, where each step actually
prints. Consider three computations of type `ListT[IO]`:

- `p` yields the two-element list `List(1, 2)` and prints nothing.
- `f(x)` prints `f(x)` and yields the one-element list `List(x)`.
- `g(y)` prints `g(y)` and yields the one-element list `List(y)`.

So `f` and `g` return their input unchanged; all that matters is *what they print*. The associativity
law claims `(p >>= f) >>= g` and `p >>= (x => f(x) >>= g)` are interchangeable. Both do compute the
same value, `List(1, 2)`. But they print in different orders:

```
(p >>= f) >>= g        prints   f(1) f(2) g(1) g(2)
p >>= (x => f(x) >>= g) prints   f(1) g(1) f(2) g(2)
```

The left grouping runs `f` over the *whole* list before `g` runs at all ("breadth first"); the right
grouping takes each element all the way through `f` and then `g` before moving on ("depth first").
Re-bracketing the binds reorders the inner effects, so the two programs are observably different and
the law fails. (If the inner effects do not depend on order — for instance the identity, reader, or
option monad — the reordering is invisible and `ListT` happens to work. It is lawful only for such
inner monads.)

#### Why this is significant

The monad laws are exactly what justify the rewriting we rely on. The desugaring of
for-comprehensions, and any refactoring that regroups a chain of `bind`s, silently assume
associativity. When it fails, two programs that Scala treats as equivalent can behave differently,
and nothing warns us. Worse, the bug only surfaces once the inner monad's effects are
order-sensitive — that is, for inner monads such as `IO` or `State`, which are precisely the ones we
most want to combine with lists. So the construction breaks in exactly the situation that motivated
building it.

#### How to fix it

The mistake is collapsing the whole list into a single `M[List[A]]`, which fuses all of the list's
effects into one lump that re-association can then regroup. The cure ("`ListT` done right") is to
interleave the effect with the *structure* of the list, producing one element at a time as an
effectful stream. Elaborating on this idea here would be a bit much. Practical libraries provide hardened
versions of this idea; the underlying fix is always the same move from "one lump of effects" to "one
effect per element."

