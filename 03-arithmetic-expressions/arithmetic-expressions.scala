import scala.language.implicitConversions

enum Exp:
  case Num(n: Int) extends Exp
  case Add(lhs: Exp, rhs: Exp) extends Exp
  case Mul(lhs: Exp, rhs: Exp) extends Exp
  case Id(x: String) extends Exp
import Exp._

val test0 = Add(Mul(Id("x"), Num(2)), Add(Id("y"), Id("y")))

implicit def num2exp(n: Int): Exp = Num(n)
implicit def sym2exp(x: String): Exp = Id(x)

val test = Add(Mul("x", 2), Add("y", "y"))

type Env = Map[String, Int]

def eval(e: Exp, env: Env): Int = e match {
  case Num(n) => n
  case Id(x) => env(x)
  case Add(l, r) => eval(l, env) + eval(r, env)
  case Mul(l, r) => eval(l, env) * eval(r, env)
}

val testEnv = Map("x" -> 3, "y" -> 4)

val exa = eval(test, testEnv)
assert(eval(test, testEnv) == 14)

case class VisitorAE[T](num: Int => T, add: (T, T) => T)
// an alternative to this design is to define num and add as abstract methods
// and then create concrete visitors by subclassing or trait composition.

enum ExpAE:
  case NumAE(n: Int) extends ExpAE
  case AddAE(lhs: ExpAE, rhs: ExpAE) extends ExpAE
import ExpAE._

def foldExp[T](v: VisitorAE[T], e: ExpAE): T = {
  e match {
    case NumAE(n) => v.num(n)
    case AddAE(l, r) => v.add(foldExp(v, l), foldExp(v, r))
  }
}

val evalVisitorAE = VisitorAE[Int](x => x, (a, b) => a + b)

def eval(e: ExpAE) = foldExp(evalVisitorAE, e)

val exaVisitorAE = eval(AddAE(AddAE(NumAE(1), NumAE(2)), NumAE(3)))
assert(exaVisitorAE == 6)

val countVisitorAE = VisitorAE[Int](_ => 1, _ + _)
val printVisitorAE = VisitorAE[String](_.toString, "(" + _ + "+" + _ + ")")

case class Visitor[T](num: Int => T, add: (T, T) => T, mul: (T, T) => T, id: String => T)
val expVisitor = Visitor[Exp](Num(_), Add(_, _), Mul(_, _), Id(_))
val countVisitor = Visitor[Int](_ => 1, _ + _, _ + _, _ => 0)
val printVisitor = Visitor[String](_.toString, "(" + _ + "+" + _ + ")", _ + "*" + _, identity)

def foldExp[T](v: Visitor[T], e: Exp): T = {
  e match {
    case Num(n) => v.num(n)
    case Add(l, r) => v.add(foldExp(v, l), foldExp(v, r))
    case Mul(l, r) => v.mul(foldExp(v, l), foldExp(v, r))
    case Id(x) => v.id(x)
  }
}

def countNums(e: Exp) = foldExp(countVisitor, e)

val exaCount = countNums(test)
assert(exaCount == 1)

val evalVisitor = Visitor[Env => Int](
   env => _ ,
   (a, b) => env =>
     a(env) + b(env),
   (a, b) => env =>
     a(env) * b(env),
   x => env =>
     env(x))

