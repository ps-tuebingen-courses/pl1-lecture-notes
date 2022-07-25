# First Class Continuations

The content of this chapter is available as a Scala file [here.](./first-class-continuations.scala)

```scala mdoc:invisible
import scala.language.implicitConversions
```


Today's goal is to formalize first-class continuations as illustrated by Scheme's let/cc construct. In the previous lecture we have
learned why first class continuations are a powerful language construct. Today we learn the semantics of first-class continuations by
extending our interpreter to support let/cc. Here is the abstract syntax of the language extended with `Letcc`:

```scala mdoc
enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp)
  case Ap(funExpr: Exp, argExpr: Exp)
  /** The abstract syntax of Letcc is as follows */
  case Letcc(param: String, body: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)
```

```scala mdoc:invisible
import Exp._
```

But how do we implement `Letcc`? How do we get hold of the rest of the computation of the object (=interpreted) program?
One idea would be to CPS-transform the object program. Then we would have the current continuation available and could
store it in environments etc.
However, we want to give a direct semantics to first-class continuations, without first  transforming the object program.

__Insight__: If we CPS-transform the interpreter, the continuation of the interpreter also represents, in some way, the continuation
of the object program. The difference is that it represents what's left to do in the interpreter and not in the object program.
However, what is left in the interpreter _is_ what is left in the object program.

Hence we are faced with two tasks:

  1. CPS-transform the interpreter
  2. add a branch for ``Letcc`` to the interpreter.

Let's start with the standard infrastructure of values and environments.

```scala mdoc
sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value
```

How do we represent values that represent continuations? Since we want to represent an object language continuation by a meta language
continuation, we need to be able to wrap a meta language continuation as an object language value. This continuation will always accept
some other object language value:

```scala mdoc
case class ContV(f: Value => Nothing) extends Value
```

We also need a syntactic construct to apply continuations. One way to provide such a construct would be to add a new syntactic category
of continuation application. We will instead do what Scheme and other languages also do: We overload the normal function applicaton
construct and also use it for application of continuations.

This means that we will need a case distinction in our interpreter whether the function argument is a closure or a continuation.
Let's now study the interpreter for our new language. The branches for `Num`, `Id`, `Add`, and `Fun` are straightforward applications of the
CPS transformation technique we already know.

```scala mdoc
def eval(e: Exp, env: Env, k: Value => Nothing): Nothing = e match {
  case Num(n: Int) => k(NumV(n))
  case Id(x) => k(env(x))
  case Add(l, r) => {
    eval(l, env, lv =>
        eval(r, env, rv =>
          (lv, rv) match {
            case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
            case _ => sys.error("can only add numbers")
          }))
  }
  case f@Fun(param, body) => k(ClosureV(f, env))

  /* In the application case we now need to distinguish whether the first argument
   * is a closure or a continuation. If it is a continuation, we ignore the
   * current continuation k and "jump" to the stored continuation by applying the
   * evaluated continuation argument to it. */
  case Ap(f, a) => eval(f, env, cl => cl match {
            case ClosureV(f, closureEnv) => eval(a, env, av => eval(f.body, closureEnv + (f.param -> av), k))
            case ContV(f) => eval(a, env, av => f(av))
            case _ => sys.error("can only apply functions")
  })
  /* Letcc is now surprisingly simple: We continue the evaluation in the body in an
   * extended environment in which param is bound to the current continuation k,
   * wrapped as a value using ContV. */
  case Letcc(param, body) => eval(body, env + (param -> ContV(k)), k)
}
```

To make it easier to experiment with the interpreter this code provides the right initialization to `eval`. We have to give `eval` a
continuation which represents the rest of the computation after `eval` is done. A small technical problem arises due to our usage of
the return type `Nothing` for continuations, to emphasize that they do not return: The only way to implement a value that has this
type is a function that does indeed not return. We do so by letting this function throw an exception. To keep track of the returned
value we store it temporarily in a variable, catch the exception, and return the stored value.

```scala mdoc
def starteval(e: Exp): Value = {
  var res: Value = null
  val s: Value => Nothing = (v) => { res = v; sys.error("program terminated") }
  try { eval(e, Map.empty, s) } catch { case e: Throwable => () }
  res
}
```

Finally a small test of `Letcc`.

```scala mdoc:silent
val testprog = Add(1, Letcc("k", Add(2, Ap("k", 3))))

assert(starteval(testprog) == NumV(4))
```

Here's a little quiz about `Letcc`:

<!-- prevent questionnaire from showing up if there is no javascript enabled-->
<noscript><style>questionnaire { display: none; }</style></noscript>
<!-- warning for user - feel free to leave out or customize -->
<noscript><div>Enable JavaScript to see the quiz</div></noscript>

<questionnaire language="en">
  <question type="singlechoice">
    What is the result of the following expression?
    <pre><code class="language-scala">
    Add(Letcc("k", Add(3, Ap("k", 1))), 4)  
    </code></pre>
    <distractor>
      <code class="language-scala">NumV(8)</code>
      <explanation>The addition of 3 is discarded.</explanation>
    </distractor>
    <solution>
      <code class="language-scala">NumV(5)</code>
    </solution>
    <distractor>
      <code class="language-scala">NumV(1)</code>
      <explanation>Don't forget the addition of 4.</explanation>
    </distractor>
    <distractor>
      <code class="language-scala">NumV(4)</code>
      <explanation>The captured continuation is the addition of 4.</explanation>
    </distractor>
  </question>
  <question type="multiplechoice">
    Which of the following assertions about let/cc are correct?
    <solution>
      let/cc captures the whole remaining computation.
    </solution>
    <solution>
      If the current continuation is not used in the body, then let/cc behaves as if it was not there.
    </solution>
    <distractor>
      The captured continuation is always used in the body of let/cc.
      <explanation>It can also be discarded.</explanation>
    </distractor>
    <distractor>
      let/cc is a control operator for delimited continuations.
      <explanation>The captured continuation is undelimited.</explanation>
    </distractor>
  </question>
</questionnaire>
