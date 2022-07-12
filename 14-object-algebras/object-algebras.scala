import scala.language.implicitConversions

trait Bool {
  def ifthenelse[T](t: T, e: T): T
}

case object T extends Bool {
  def ifthenelse[T](t: T, e: T) = t
}
case object F extends Bool {
  def ifthenelse[T](t: T, e: T) = e
}
def and(a: Bool, b: Bool): Bool = a.ifthenelse(b, a)

trait NumC {
  def fold[T](z: T, s: T => T): T
}
case object Zero extends NumC {
  def fold[T](z: T, s: T => T) = z
}

case class Succ(n: NumC) extends NumC {
  def fold[T](z: T, s: T => T) = s(n.fold(z, s))
}

def plus(a: NumC, b: NumC) = {
  new NumC {
    def fold[T](z: T, s: T => T): T = {
      a.fold(b.fold(z, s), s)
    }
  }
}

val oneC = Succ(Zero)
val twoC = Succ(oneC)
val threeC = Succ(twoC)

def testplusC = plus(twoC, threeC).fold[Unit]((), _ => print("."))

trait NumSig[T] {
  def z: T
  def s(p: T): T
}

trait Num {
  def apply[T](x: NumSig[T]): T
}

def plus(a: Num, b: Num) = new Num {
  def apply[T](x: NumSig[T]): T = a(new NumSig[T] {
    def z = b(x)
    def s(p: T) = x.s(p)
  })
}

val zero: Num = new Num { def apply[T](x: NumSig[T]) = x.z }
val one: Num = new Num { def apply[T](x: NumSig[T]) = x.s(x.z) }
val two: Num = new Num { def apply[T](x: NumSig[T]) = x.s(one(x)) }
val three: Num = new Num { def apply[T](x: NumSig[T]) = x.s(two(x)) }

object NumAlg extends NumSig[Int] {
  def z = 0
  def s(x: Int) = x + 1
}

val testplus = plus(two, three)(NumAlg)

trait Exp[T] {
  implicit def id(name: String): T
  def fun(param: String, body: T): T
  def ap(funExpr: T, argExpr: T): T
  implicit def num(n: Int): T
  def add(e1: T, e2: T): T
  def wth(x: String, xdef: T, body: T): T = ap(fun(x, body), xdef)
}

sealed abstract class Value
type Env = Map[String, Value]
case class ClosureV(f: Value => Value) extends Value
case class NumV(n: Int) extends Value

trait eval extends Exp[Env => Value] {
  def id(name: String) = env => env(name)
  def fun(param: String, body: Env => Value) = env => ClosureV(v => body(env + (param -> v)))
  def ap(funExpr: Env => Value, argExpr: Env => Value) = env => funExpr(env) match {
    case ClosureV(f) => f(argExpr(env))
    case _ => sys.error("can only apply functions")
  }
  def num(n: Int) = env => NumV(n)
  def add(e1: Env => Value, e2: Env => Value) = env => (e1(env), e2(env)) match {
    case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
    case _ => sys.error("can only add numbers")
  }
}

object eval extends eval

def test[T](semantics: Exp[T]) = {
  import semantics._

  ap(ap(fun("x", fun("y", add("x", "y"))), 5), 3)
}

val testres = test(eval)(Map.empty)

trait ExpWithMult[T] extends Exp[T] {
  def mult(e1: T, e2: T): T
}

trait evalWithMult extends eval with ExpWithMult[Env => Value] {
  def mult(e1: Env => Value, e2: Env => Value) = env => (e1(env), e2(env)) match {
    case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
    case _ => sys.error("can only multiply numbers")
  }
}
object evalWithMult extends evalWithMult

def testMult[T](semantics: ExpWithMult[T]) = {
  import semantics._

  ap(ap(fun("x", fun("y", mult("x", "y"))), 5), 3)
}

val testresMult = testMult(evalWithMult)(Map.empty)

trait ExpT {
  type Rep[_]
  def fun[S, T](f: Rep[S] => Rep[T]): Rep[S => T]
  def ap[S, T](funExpr: Rep[S => T], argExpr: Rep[S]): Rep[T]
  implicit def num(n: Int): Rep[Int]
  def add(e1: Rep[Int], e2: Rep[Int]): Rep[Int]
}

object evalT extends ExpT {
  type Rep[X] = X
  def fun[S, T](f: S => T) = f
  def ap[S, T](f: S => T, a: S) = f(a)
  def num(n: Int) = n
  def add(e1: Int, e2: Int) = e1 + e2
}

object prettyprintT extends ExpT {
  var counter = 0
  type Rep[X] = String
  def fun[S, T](f: String => String) = {
    val varname = "x" + counter.toString
    counter += 1
    "(" + varname + " => " +  f(varname) + ")"
  }
  def ap[S, T](f: String, a: String) = f + "(" + a + ")"
  def num(n: Int) = n.toString
  def add(e1: String, e2: String) = "(" + e1 + "+" + e2 + ")"
}

def test2(semantics: ExpT) = {
  import semantics._
  ap(ap(fun((x: Rep[Int]) => fun((y: Rep[Int]) => add(x, y))), 5), 3)
}

val testres2 = test2(evalT)
val testres3 = test2(prettyprintT)

def testilltyped(semantics: ExpT) = {
  import semantics._
  add(5, fun((x: Rep[Int]) => x))
}

