enum Exp:
  case Num(n : Int)
  case Id(name : String)
  case Add(lhs : Exp, rhs : Exp)
  case Fun(param : String, body : Exp)
  case Ap (funExpr : Exp, argExpr : Exp)
import Exp._

sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n : Int) extends Value
case class ClosureV(f : Fun, env : Env) extends Value

object CPSTransformed {
  def eval[T](e : Exp, env : Env, k : Value => T) : T = e match {
    case Num(n : Int) => k(NumV(n))
    case Id(x) => k(env(x))
    case Add(l, r) =>
      eval( l, env
          , lv => eval( r, env
                      , rv => (lv, rv) match {
                          case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
                          case _ => sys.error("can only add numbers")
                        } ) )
    case f@Fun(param, body) => k(ClosureV(f, env))
    case Ap(f, a) =>
      eval( f, env
          , cl => cl match {
              case ClosureV(f, closureEnv) =>
                eval( a, env
                    , av => eval(f.body, closureEnv + (f.param -> av), k) )
              case _ => sys.error("can only apply functions")
            } )
  }
}

def map(f : Int => Int, xs : List[Int]) : List[Int] = xs match {
  case Nil => Nil
  case (x :: xs) => f(x) :: map(f, xs)
}

def addAndMultNToList(n : Int, xs : List[Int]) = map(y => y * n, map(y => y + n, xs))

def fLam(n : Int) = (y : Int) => y + n
def gLam(n : Int) = (y : Int) => y * n

def f(n : Int)(y : Int) = y + n
def g(n : Int)(y : Int) = y * n

def addAndMultNToListLifted(n : Int, xs : List[Int]) = map(g(n)(_), map(f(n)(_), xs))

object LambdaLifted {
  def addc1[T](r : Exp, env : Env, k : Value => T)(lv : Value) = eval(r, env, addc2(lv, k))

  def addc2[T](lv : Value, k : Value => T)(rv : Value) = (lv, rv) match {
    case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
    case _ => sys.error("can only add numbers")
  }

  def appc1[T](a : Exp, env : Env, k : Value => T)(cl : Value) = cl match {
    case ClosureV(f, closureEnv) => eval(a, env, appc2(f, closureEnv, k))
    case _ => sys.error("can only apply functions")
  }

  def appc2[T](f : Fun, closureEnv : Env, k : Value => T)(av : Value) = eval(f.body, closureEnv + (f.param -> av), k)

  def eval[T](e : Exp, env : Env, k : Value => T) : T = e match {
    case Num(n : Int) => k(NumV(n))
    case Id(x) => k(env(x))
    case Add(l, r) => eval(l, env, addc1(r, env, k))
    case f@Fun(param, body) => k(ClosureV(f, env))
    case Ap(f, a) => eval(f, env, appc1(a, env, k))
  }
}

sealed abstract class FunctionValue
case class F(n : Int) extends FunctionValue
case class G(n : Int) extends FunctionValue

def apply(f : FunctionValue, y : Int) : Int = f match {
  case F(n) => y + n
  case G(n) => y * n
}

def map(f : FunctionValue, xs : List[Int]) : List[Int] = xs match {
  case Nil => Nil
  case (x :: xs) => apply(f, x) :: map(f, xs)
}

def addAndMultNToListDefun(n : Int, xs : List[Int]) = map(G(n), map(F(n), xs))

object Defunctionalized {

  sealed abstract class FunctionValue[T]
  case class AddC1[T](r : Exp, env : Env, k : FunctionValue[T]) extends FunctionValue[T]
  case class AddC2[T](lv : Value, k : FunctionValue[T]) extends FunctionValue[T]
  case class ApC1[T](a : Exp, env : Env, k : FunctionValue[T]) extends FunctionValue[T]
  case class ApC2[T](f : Fun, closureEnv : Env, k : FunctionValue[T]) extends FunctionValue[T]

  def apply[T](fv : FunctionValue[T], v : Value) : T  = fv match {
    case AddC1(r, env, k) => eval(r, env, AddC2(v, k))
    case AddC2(lv, k) => (lv, v) match {
      case (NumV(v1), NumV(v2)) => apply(k, NumV(v1 + v2))
      case _ => sys.error("can only add numbers")
    }
    case ApC1(a, env, k) => v match {
      case ClosureV(f, closureEnv) => eval(a, env, ApC2(f, closureEnv, k))
      case _ => sys.error("can only apply functions")
    }
    case ApC2(f, closureEnv, k) => eval(f.body, closureEnv + (f.param -> v), k)
  }

  def eval[T](e : Exp, env : Env, k : FunctionValue[T]) : T = e match {
    case Num(n : Int) => apply(k, NumV(n))
    case Id(x) => apply(k, env(x))
    case Add(l, r) => eval(l, env, AddC1(r, env, k))
    case f@Fun(param, body) => apply(k, ClosureV(f, env))
    case Ap(f, a) => eval(f, env, ApC1(a, env, k))
  }
}

