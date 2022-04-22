import scala.language.implicitConversions

object AEId {
```scala mdoc
val test0 = Add(Mul(Id("x"),Num(2)),Add(Id("y"),Id("y")))

implicit def num2exp(n: Int) = Num(n)
implicit def sym2exp(x: String) = Id(x)

val test = Add(Mul("x",2),Add("y","y"))

type Env = Map[String,Int]

def eval(e: Exp, env: Env) : Int = e match {
  case Num(n) => n
  case Id(x) => env(x)
  case Add(l,r) => eval(l,env) + eval(r,env)
  case Mul(l,r) => eval(l,env) * eval(r,env)
}

val testEnv = Map("x" -> 3, "y" -> 4)

val exa = eval(test, testEnv)
assert(eval(test, testEnv) == 14)
```scala
object Visitors {
```scala mdoc
sealed abstract class ExpAE

case class NumAE(n: Int) extends ExpAE
case class AddAE(lhs: ExpAE, rhs: ExpAE) extends ExpAE

def foldExp[T](v: VisitorAE[T], e: ExpAE): T = {
  e match {
    case NumAE(n) => v.num(n)
    case AddAE(l, r) => v.add(foldExp(v, l), foldExp(v, r))
  }
}

val evalVisitorAE = VisitorAE[Int](x => x, (a, b) => a + b)

def eval(e: ExpAE) = foldExp(evalVisitorAE, e)

val exaVisitorAE = eval(AddAE(AddAE(NumAE(1),NumAE(2)),NumAE(3)))
assert(exaVisitorAE == 6)

val countVisitorAE = VisitorAE[Int]( _=>1, _+_)
val printVisitorAE = VisitorAE[String](_.toString, "("+_+"+"+_+")")
```scala
object AEIdVisitor {
import AEId._
```scala mdoc:silent
def countNums(e: Exp) = foldExp(countVisitor, e)

val exaCount = countNums(test)
assert(exaCount == 1)

val evalVisitor = Visitor[Env=>Int](
   env => _ ,
   (a, b) => env =>
     a(env) + b(env),
   (a, b) => env =>
     a(env) * b(env),
   x => env =>
     env(x))
```scala
