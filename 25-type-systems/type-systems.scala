import scala.language.implicitConversions

enum Exp:
  case Num(n: Int)
  case Add(lhs: Exp, rhs: Exp)
  case Bool(x: Boolean)
  case If(cond: Exp, thn: Exp, els: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def bool2exp(x: Boolean): Exp = Bool(x)

import Exp._

def eval(e: Exp): Exp = e match {
  case Add(l, r) => (eval(l), eval(r)) match {
                     case (Num(x), Num(y)) => Num(x + y)
                     case _ => sys.error("can only add numbers")
                    }
  case If(cond, thn, els) =>
    eval(cond) match {
      case Bool(true) => eval(thn)
      case Bool(false) => eval(els)
      case _ => sys.error("Condition must be boolean")
    }
  case _ => e
}

val ex1 = Add(3,5)
val ex2 = If(true, 7, 5)
val ex2b = If(true, 7, true)
val ex3 = If(Add(3,4), 7, true)
val ex4 = Add(true,3)

enum Type:
  case BoolType()
  case IntType()
import Type._

def typeCheck(e: Exp): Type = e match {
  case Num(n) => IntType()
  case Bool(x) => BoolType()
  case Add(a, b) => (typeCheck(a), typeCheck(b)) match {
    case (IntType(), IntType()) => IntType()
    case _ => sys.error("Type Error in addition")
  }
  case If(c, t, e) => (typeCheck(c), typeCheck(t), typeCheck(e)) match {
    case (BoolType(), t1, t2) => if (t1 == t2) t1 else sys.error("Type error in if")
    case _ => sys.error("Type error in If")
  }
}

