# IO Monad

The content of this chapter is available as a Scala file [here.](./io-monad.scala)

Input and output sit uneasily with pure functional programming. In a pure language an expression
denotes only a value, and denotes the _same_ value however often it is evaluated (referential
transparency). But printing to the screen or reading a line from the keyboard is a _side effect_ that
happens in time and cannot be freely duplicated or reordered. How can such a language talk to the
outside world without giving up its purity? The _IO monad_ is the now-standard answer.

A Little History
----------------

Several answers were tried before the monadic one prevailed. The _lazy stream_ (or "lazy I/O") model
makes a program a function from a lazily-produced stream of input characters to a stream of output
characters, with the runtime supplying and consuming characters on demand. The related _dialogue_
style of early Haskell made a program a lazy list of requests to the operating system paired with a
lazy list of responses. Both are genuinely pure, but fragile: the types do not tie a response to its
request, and laziness makes the order and timing of effects hard to predict. The monadic formulation of
Peyton Jones and Wadler ("Imperative Functional Programming", POPL 1993) — building on Moggi's use of
monads to model computational effects and Wadler's advocacy of monads for structuring functional
programs — replaced these with a typed, composable discipline in which the sequencing of effects is
explicit. It is the design Haskell uses today.

Plans, and Running Them
-----------------------

The central idea is to _separate the description of an effect from its execution_. A value of type
`IO[A]` is not an effect that has already happened; it is a **plan** — a first-class description of a
computation that, _when performed_, will interact with the world and deliver a value of type `A`.
Constructing and combining plans is pure: evaluating `printString("hi")` prints nothing, just as writing
down a recipe cooks no food. Effects occur only when a plan is finally _run_, and there is exactly one
place where that happens: the plan named `main` (or, interactively, an expression entered at the REPL),
which the runtime takes and performs. A whole program is thus a pure expression that _computes_ one big
IO plan; the single impure act is the runtime feeding that plan the real world.

We capture this as an interface. It is the monad of the previous chapter — `unit` and `bind` — enriched
with two primitive effects and, crucially, a way to run a plan:

```scala mdoc
trait IOMonad {
  type IO[_]
  def unit[A](a: A): IO[A]
  def bind[A, B](m: IO[A], f: A => IO[B]): IO[B]
  def printString(s: String): IO[Unit]
  def inputString: IO[String]

  def performIO[A](action: IO[A]): A
}
```

`unit(a)` is the trivial plan "do nothing, yield `a`"; `bind(m, f)` is "perform `m`, then perform the
plan that `f` builds from its result", which is what forces one effect to come after another.
`printString` and `inputString` are the primitive plans, and `performIO` is the runner that stands in
for the language runtime.

Threading the World Through
---------------------------

How can a _pure_ function stand for an effect? By making the world an explicit argument that is threaded
through the computation: an IO action is a function `World => (A, World)` that receives the state of the
world before the action and returns the result together with the world after it. This is precisely the
**State monad** of the previous chapter, with the state fixed to be "the world" — and since each action
consumes the old world and produces a new one, the actions are forced into a definite order.

```scala mdoc:silent
val iomonad: IOMonad = new IOMonad {
  type World = String
  type IO[A] = World => (A, World)
  def unit[A](a: A): IO[A] = w => (a, w)
  def bind[A, B](m: IO[A], f: A => IO[B]): IO[B] =
    w => m(w) match { case (a, w2) => f(a)(w2) }
  def printString(s: String): IO[Unit] =
    w => { println(s); ((), w + s + " was printed and then ...\n") }
  def inputString: IO[String] =
    w => {
      val input = scala.io.StdIn.readLine();
      (input, w + input + " was entered and then ...\n")
    }

  def performIO[A](action: IO[A]): A =
    action("The world in which nothing has happened yet, but then ...\n") match {
      case (a, w) =>
        println("Peformed all actions. The world in which all this happened is: \n" + w); a
    }
}
```

Here `World` is modelled, purely for illustration, as a `String` that logs what has happened. `performIO`
plays the role of "reaching `main`": it manufactures an initial world ("nothing has happened yet"), hands
it to the plan to set the chain of effects in motion, and returns the final result. Note that the
`println` inside `printString` runs only when the resulting function is _applied_ — that is, during
`performIO` — so up to that point we have genuinely built nothing but a description.

```scala mdoc
def someIOActions(implicit m: IOMonad): m.IO[Unit] =
  m.bind(m.printString("Enter your first name:"), (_: Unit) =>
  m.bind(m.inputString, (firstName: String) =>
  m.bind(m.printString("Enter your last name:"), (_: Unit) =>
  m.bind(m.inputString, (lastName: String) =>
  m.printString("Hello, " + firstName + " " + lastName + "!")))))

def test = iomonad.performIO(someIOActions(iomonad))
```

`someIOActions` is an ordinary, pure value of type `IO[Unit]`, assembled with `bind`; it does nothing
until `test` runs it through `performIO`. GHC's real `IO` works on the same principle, defining `IO a`
essentially as `World -> (World, a)` for an abstract, unforgeable world token: the world is never truly
copied around at run time, but the type enforces the sequencing, and effects still, conceptually, happen
only when the plan reaches `main`.
