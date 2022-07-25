# Desugaring

The content of this chapter is available as a Scala file [here.](./desugaring.scala)

## Desugaring

You have already seen the basic structure of an interpreter by means of an interpreter for a language of arithmetic `Exps`:

```scala mdoc
object AE {
  // Abstract Syntax Tree
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
  import Exp._

  // Example
  val ex = Add(Num(1), Add(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) =>
        eval(lhs) + eval(rhs)
    }
}
```

In this lecture we want to study the technique of desugaring as a means to structure programming languages and decompose a language into
a core language and syntactic sugar.

For illustration, consider the following proposed extensions to the language:
  1. `Mult`
  2. `Sub`
  3. Unary Negation

Extension number 1 is a good example for a core language extension. We have no way of expressing `Mult` in terms of the existing constructs
(if we had some looping construct we could express `Mult` as repeated `Add` but we do not have loops).

Hence we add this language construct to the (core) language:

```scala mdoc
object MAE {
  // Abstract Syntax Tree
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
    case Mult(lhs: Exp, rhs: Exp) extends Exp
  import Exp._
  // Example
  val ex = Add(Num(1), Mult(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}
```

Let us now consider extension #2, `Sub`. One way to support sub is to add it to the core language, just like `Mult`:

```scala mdoc
object SMAE {
  // Abstract Syntax Tree
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
    case Mult(lhs: Exp, rhs: Exp) extends Exp
    case Sub(lhs: Exp, rhs: Exp) extends Exp
  import Exp._
  // Example
  val ex = Sub(Num(1), Mult(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
      case Sub(lhs, rhs) =>  eval(lhs) - eval(rhs)
    }
}
```

However, another way of adding sub is to treat it as syntactic sugar using the fact that ``a - b = a + (-1 * b)``
One way of expressing the desugaring is as a syntax transformation:

```scala mdoc
def desugarSMAE2MAE(e: SMAE.Exp): MAE.Exp = e match {
  case SMAE.Exp.Num(n) => MAE.Exp.Num(n)
  case SMAE.Exp.Add(lhs, rhs) => MAE.Exp.Add(desugarSMAE2MAE(lhs), desugarSMAE2MAE(rhs))
  case SMAE.Exp.Mult(lhs, rhs) => MAE.Exp.Mult(desugarSMAE2MAE(lhs), desugarSMAE2MAE(rhs))
  case SMAE.Exp.Sub(lhs, rhs) =>
    MAE.Exp.Add(desugarSMAE2MAE(lhs),
                 MAE.Exp.Mult(MAE.Exp.Num(-1), desugarSMAE2MAE(rhs)))
}
```

With this desugaring in place, we do not need an interpreter for SMAE anymore; rather we can reuse the MAE interpreter:

```scala mdoc
val res = MAE.eval(desugarSMAE2MAE(SMAE.ex))
```

If we had written other algorithms on MAE, or had proven properties of MAE, they'd be applicable to SMAE, too. Hence desugaring is a way
of reusing code, proofs, ... . It is important, though, that the desugared language feature is gone after desugaring. For instance,
a pretty printer would print the desugared code. A debugger would use the desugared code. This can be an important downside to desugaring.
There are ways to avoid or mitigate these shortcomings, but they require additional work.
There is a second way of realizing desugaring which does not require the definition of a copy of the AST classes. We can desugar earlier,
namely during the construction of the AST:

```scala mdoc
object SMAE2 {
  // Abstract Syntax Tree
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
    case Mult(lhs: Exp, rhs: Exp) extends Exp
  object Exp:
    def sub(e1: Exp, e2: Exp): Exp =
      Add(e1, Mult(Num(-1), e2))
  import Exp._

  // Compared to SMAE, we only have to change upper case Sub by lower case sub
  // when constructing examples.
  val ex = sub(Num(1), Mult(Num(5), Num(3)))

  // Interpreter - no case for sub needed
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}
```

Let us now consider the third extension, unary minus. Here we have three choices:

 1. Add unary minus to the core language
 2. Treat unary minus as syntactic sugar for the core language using  ``-x = (-1)*x``
 3. Treat unary minus as syntactic sugar on top of the syntactic sugar using ``-x = 0 - x``.

We will use the third option to illustrate that one can build layers of syntactic sugar:

```scala mdoc
object USMAE {
  // Abstract Syntax Tree
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
    case Mult(lhs: Exp, rhs: Exp) extends Exp
  object Exp:
    def sub(e1: Exp, e2: Exp): Exp =
      Add(e1, Mult(Num(-1), e2))
    def unaryminus(e: Exp) = sub(Num(0), e)
  import Exp._

  // Compared to SMAE, we only have to change upper case Sub by lower case sub
  // when constructing examples.
  val ex = sub(unaryminus(Num(1)), Mult(Num(5), Num(3)))

  // Interpreter - no case for sub needed
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}
```

<!-- prevent questionnaire from showing up if there is no javascript enabled-->
<noscript><style>questionnaire { display: none; }</style></noscript>
<!-- warning for user - feel free to leave out or customize -->
<noscript><div>Enable JavaScript to see the quiz</div></noscript>

<questionnaire language="en">
  <question type="singlechoice">
    What is the result of desugaring the following expression in the language USMAE?
    <pre><code class="language-scala">
  sub(unaryminus(Num(1)), Num(7))
    </code></pre>
    <distractor>
      <pre><code class="language-scala">
  sub(unaryminus(Num(1)), Num(7))
      </code></pre>
      <explanation>No desugaring was applied.</explanation>
    </distractor>
    <distractor>
      <pre><code class="language-scala">
  Num(-8)
      </code></pre>
      <explanation>Desugaring does not mean evaluation.</explanation>
    </distractor>
    <distractor>
      <pre><code class="language-scala">
  sub(sub(Num(0), Num(1)), Num(7))
      </code></pre>
      <explanation><code>sub</code> has to be desugared, too.</explanation>
    </distractor>
    <solution>
      <pre><code class="language-scala">
  Add(Add(Num(0), Mult(Num(-1), Num(1))),
      Mult(Num(-1), Num(7)))
      </code></pre>
    </solution>
  </question>
</questionnaire>
