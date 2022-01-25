# Arithmetic Expressions With Variables

```scala mdoc:invisible
import scala.language.implicitConversions
```

Let us now consider an extension of the arithmetic expression language with variables. We do this by a new kind of expression, which we
call Identifier, or Id.

\\[
  \begin{array}{lclr}
    e & := & n & \textit{Numeric Literals} \\\\
    & | & (e + e) & \textit{Addition} \\\\
    & | & (e * e) & \textit{Multiplication} \\\\
    & | & x & \textit{Identifier}
  \end{array}
\\]


```scala mdoc
object AEId {

sealed abstract class Exp

case class Num(n: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Mul(lhs: Exp, rhs: Exp) extends Exp
case class Id(x: String) extends Exp
// Strings are similar to strings, but they are implicitly "canonicalized"
// (meaning all instances of the same String point to a unique copy, in other words,
// they are not cloned but shared) and  can hence be compared efficiently.
// Unlike strings, they can not be manipulated.
// They can be constructed by prefixing an identifier with a '

/**
Here is a sample program written in this language, directly written down using case class constructors
*/

 val test0 = Add(Mul(Id("x"),Num(2)),Add(Id("y"),Id("y")))

/**
With a proper parser we could choose a syntax like "x*2+y+y". We do not care much about concrete syntax and parsing, though. That said,
to make writing examples less verbose, Scala's implicits come to the rescue.
Calls to implicit functions are inserted automatically by the compiler if they help to restore well-typedness. For instance, we can define:
*/
implicit def num2exp(n: Int) = Num(n)
implicit def sym2exp(x: String) = Id(x)

/**
to lift integers and Strings to expressions. Using these implicits, the example can be written as: */

 val test = Add(Mul("x",2),Add("y","y"))

/**
To give meaning to identifiers, we use _environments_. Environments are mappings from Identifiers (which we represent as Strings) to Values.
In our simple language the only values are integers, hence:
*/

type Env = Map[String,Int]

/**
An evaluator (or interpreter) for this language takes an expression and an environment as parameter and produces a value - in this case
"Int". This interpreter uses pattern matching over case classes.
*/

 def eval(e: Exp, env: Env) : Int = e match {
  case Num(n) => n
  case Id(x) => env(x)
  case Add(l,r) => eval(l,env) + eval(r,env)
  case Mul(l,r) => eval(l,env) * eval(r,env)
}

/**
A different (and arguably more 'object-oriented') way to implement this evaluator would be to add an abstract "eval" method to the Exp
class and override it in all subclasses, each implementation corresponding to its corresponding case in the pattern match. The choice
between these alternatives matters, since they support different dimensions of extensibility.
We will mainly use the more functional style using pattern matching, because it matches better to the order in which we present these
topics in the lecture. To try the example, we need a sample environment that gives values to the (free) variables in the sample expression.
The test environment also illustrates how Scala supports direct definitions of constant maps.
*/

val testEnv = Map("x" -> 3, "y" -> 4)

/**
We can automatically test our evaluator using assert :
*/
assert(eval(test, testEnv) == 14)

}
```

We will now learn a different way to encode algorithms that operate on expressions (like the evaluator). To this end, we will now use
so-called "folds". Folds are well-known for lists, but the concept is more general and applies to arbitrary algebraic data types.
We will present folds in such a style that they resemble visitors as known from the OO design pattern literature. They correspond to
so-called "internal visitors" in which the traversal is encoded within the "accept" function.
An internal visitor consists of one function for each syntactic construct of the language. It has a type parameter that determines the
"return type" of invoking the visitor on an expression. This type parameter is used in all positions in which the original syntax
specifies a subexpression.
Internal visitors also correspond to a "bottom-up" traversal of the syntax tree.

```scala mdoc
object Visitors {
  case class Visitor[T](num: Int => T, add: (T, T) => T)
  // an alternative to this design is to define num and add as abstract methods
  // and then create concrete visitors by subclassing or trait composition.

/**
The fold function itself applies a visitor to an expression. Note that the recursion is performed in the fold function, hence all visitors
are not recursive.
Also note that this design enforces that all algorithms specified via this visitor interfaces are compositional by design. This means that
the recursion structure of the algorithm corresponds to the recursion structure of the expression. Put in another way, it means that the
semantics (in terms of the meta-language) of a composite expression is determined by the semantics of the subexpressions; the syntax of
the subexpressions is irrelevant.
Compositional specifications are particularly nice because they enable "equational reasoning": Subexpressions can be replaced by other
subexpressions with the same semantics without changing the semantics of the whole.
*/

  sealed abstract class Exp

  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp

  def foldExp[T](v: Visitor[T], e: Exp): T = {
    e match {
      case Num(n) => v.num(n)
      case Add(l, r) => v.add(foldExp(v, l), foldExp(v, r))
    }
  }

/**
Here is our evaluator from above rephrased using the visitor infrastructure.
*/

  val evalVisitor = Visitor[Int](x => x, (a, b) => a + b)

/**
We can of course also restore the original interface of eval
*/

  def eval(e: Exp) = foldExp(evalVisitor, e)

/**
Let's test whether it works.
*/

  assert(eval(Add(Add(Num(1),Num(2)),Num(3))) == 6)

/**
We can of course also apply other algorithms using visitors, such as counting the number of "Num" literals, or printing to a string:
*/
  val countVisitor = Visitor[Int]( _=>1, _+_)
  val printVisitor = Visitor[String](_.toString, "("+_+"+"+_+")")

}
```

Let's now try the same with the AE language with identifiers. It all works in the same way:

```scala mdoc
object AEIdVisitor {
  import AEId._

  case class Visitor[T](num: Int => T, add: (T, T) => T, mul: (T, T) => T, id: String => T)
  val expVisitor = Visitor[Exp](Num(_), Add(_, _), Mul(_, _), Id(_))
  val countVisitor = Visitor[Int](_=>1, _ + _, _ + _, _ => 0)
  val printVisitor = Visitor[String](_.toString, "(" + _ + "+" + _ + ")", _ + "*" + _, _.x)

  def foldExp[T](v: Visitor[T], e: Exp) : T = {
    e match {
      case Num(n) => v.num(n)
      case Add(l,r) => v.add(foldExp(v, l), foldExp(v, r))
      case Mul(l,r) => v.mul(foldExp(v, l), foldExp(v, r))
      case Id(x) => v.id(x)
    }
  }

  def countNums(e: Exp) = foldExp(countVisitor, e)

  assert(countNums(test) == 1)

/**
However, what about the evaluator? If we instantiate T = Int, then how can we access the environment? Insight: For evaluation, we must
instantiate T with a function type ``Env => Int``! This way of transforming a multi-argument function into a single-argument
higher-order function is called _currying_.
*/

  val evalVisitor = Visitor[Env=>Int](
     env => _ ,
     (a, b) => env =>
       a(env) + b(env),
     (a, b) => env =>
       a(env) * b(env),
     x => env =>
       env(x))
}
```