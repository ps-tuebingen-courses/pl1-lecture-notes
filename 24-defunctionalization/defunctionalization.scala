enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp)
  case Ap(funExpr: Exp, argExpr: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)

import Exp._

sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

object CPSTransformed {
  def eval[T](e: Exp, env: Env, k: Value => T): T = e match {
    case Num(n: Int) => k(NumV(n))
    case Id(x) => k(env(x))
    case Add(l, r) =>
      eval(l, env,
           lv => eval(r, env,
                      rv => (lv, rv) match {
                          case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
                          case _ => sys.error("can only add numbers")
                        }))
    case f@Fun(param, body) => k(ClosureV(f, env))
    case Ap(f, a) =>
      eval(f, env,
           cl => cl match {
              case ClosureV(f, closureEnv) =>
                eval(a, env,
                     av => eval(f.body, closureEnv + (f.param -> av), k))
              case _ => sys.error("can only apply functions")
            })
  }
}

def map(f: Int => Int, xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case (x :: xs) => f(x) :: map(f, xs)
}

def addAndMultNToList(n: Int, xs: List[Int]) = map(y => y * n, map(y => y + n, xs))

def fLam(n: Int) = (y: Int) => y + n
def gLam(n: Int) = (y: Int) => y * n

def f(n: Int)(y: Int) = y + n
def g(n: Int)(y: Int) = y * n

def addAndMultNToListLifted(n: Int, xs: List[Int]) = map(g(n)(_), map(f(n)(_), xs))

object LambdaLifted {
  def addC1[T](r: Exp, env: Env, k: Value => T)(lv: Value) =
    eval(r, env, addC2(lv, k))

  def addC2[T](lv: Value, k: Value => T)(rv: Value) = (lv, rv) match {
    case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
    case _ => sys.error("can only add numbers")
  }

  def apC1[T](a: Exp, env: Env, k: Value => T)(cl: Value) = cl match {
    case ClosureV(f, closureEnv) => eval(a, env, apC2(f, closureEnv, k))
    case _ => sys.error("can only apply functions")
  }

  def apC2[T](f: Fun, closureEnv: Env, k: Value => T)(av: Value) =
    eval(f.body, closureEnv + (f.param -> av), k)

  def eval[T](e: Exp, env: Env, k: Value => T): T = e match {
    case Num(n: Int) => k(NumV(n))
    case Id(x) => k(env(x))
    case Add(l, r) => eval(l, env, addC1(r, env, k))
    case f@Fun(param, body) => k(ClosureV(f, env))
    case Ap(f, a) => eval(f, env, apC1(a, env, k))
  }
}

enum FunctionValue:
  case F(n: Int)
  case G(n: Int)

import FunctionValue._

def apply(f: FunctionValue, y: Int): Int = f match {
  case F(n) => y + n
  case G(n) => y * n
}

def map(f: FunctionValue, xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case (x :: xs) => apply(f, x) :: map(f, xs)
}

def addAndMultNToListDefun(n: Int, xs: List[Int]) = map(G(n), map(F(n), xs))

object Defunctionalized {

  enum FunctionValue[T]:
    case AddC1(r: Exp, env: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case AddC2(lv: Value, k: FunctionValue[T]) extends FunctionValue[T]
    case ApC1(a: Exp, env: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case ApC2(f: Fun, closureEnv: Env, k: FunctionValue[T]) extends FunctionValue[T]

  import FunctionValue._

  def apply[T](fv: FunctionValue[T], v: Value): T  = fv match {
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

  def eval[T](e: Exp, env: Env, k: FunctionValue[T]): T = e match {
    case Num(n: Int) => apply(k, NumV(n))
    case Id(x) => apply(k, env(x))
    case Add(l, r) => eval(l, env, AddC1(r, env, k))
    case f@Fun(param, body) => apply(k, ClosureV(f, env))
    case Ap(f, a) => eval(f, env, ApC1(a, env, k))
  }
}

object AbstractMachine {
  enum FunctionValue[T]:
    case AddC1(r: Exp, env: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case AddC2(lv: Value, k: FunctionValue[T]) extends FunctionValue[T]
    case ApC1(a: Exp, env: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case ApC2(f: Fun, closureEnv: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case IdentityFV() extends FunctionValue[Value]

  import FunctionValue._

  enum MachineState[T]:
    case EvalState[T](e: Exp, env: Env, fv: FunctionValue[T]) extends MachineState[T]
    case ApplyState[T](fv: FunctionValue[T], v: Value) extends MachineState[T]
    case Done(v: Value) extends MachineState[Value]

  import MachineState._

  def transition[T](s: MachineState[T]): MachineState[T] =
    s match {
      case EvalState(e, env, k) => transitionEval(e, env, k)
      case ApplyState(fv, v) => transitionApply(fv, v)
      case Done(v) => sys.error("already done")
    }

  def transitionEval[T](e: Exp, env: Env, k: FunctionValue[T]): MachineState[T] = e match {
    case Num(n: Int) => ApplyState(k, NumV(n))
    case Id(x) => ApplyState(k, env(x))
    case Add(l, r) =>
      EvalState(l, env, AddC1(r, env, k))
    case f@Fun(param, body) => ApplyState(k, ClosureV(f, env))
    case Ap(f, a) =>  EvalState(f, env, ApC1(a, env, k))
   }

   def transitionApply[T](fv: FunctionValue[T], v: Value): MachineState[T] = fv match {
    case IdentityFV() => Done(v)
    case AddC1(r, env, k) => EvalState(r, env, AddC2(v, k))
    case AddC2(lv, k) => (lv, v) match {
          case (NumV(v1), NumV(v2)) => ApplyState(k, NumV(v1 + v2))
          case _ => sys.error("can only add numbers")
         }
    case ApC1(a, env, k) => v match {
        case ClosureV(f, closureEnv) => EvalState(a, env, ApC2(f, closureEnv, k))
        case _ => sys.error("can only apply functions")
    }
    case ApC2(f, closureEnv, k) => EvalState(f.body, closureEnv + (f.param -> v), k)
  }
}
import AbstractMachine._
import FunctionValue._
import MachineState._

val test = Ap(Fun("x", Add("x", 1)), 5)
val initMS: MachineState[Value] = EvalState(test, Map.empty, AbstractMachine.FunctionValue.IdentityFV())
val s1 = transition(initMS)
val s2 = transition(s1)
val s3 = transition(s2)
val s4 = transition(s3)
val s5 = transition(s4)
val s6 = transition(s5)
val s7 = transition(s6)
val s8 = transition(s7)
val s9 = transition(s8)
val s10 = transition(s9)
val s11 = transition(s10)

def evalMachine(e: Exp): List[MachineState[Value]] = {
  val initMS: MachineState[Value] = EvalState(e, Map.empty, AbstractMachine.FunctionValue.IdentityFV())
  List.unfold(initMS)({ case Done(v) => None
                        case s => { val s2 = transition(s); Some((s, s2))}})
}

val q = evalMachine(test)

enum MyList[T]:
  case MyEmptyList() extends MyList[T]
  case MyCons(x: T, xs: MyList[T]) extends MyList[T]

import MyList._

def nth[T](l: MyList[T], i: Int): T = l match {
  case MyEmptyList() => sys.error("index out of range")
  case MyCons(x, xs) => if (i == 0) then x else nth(xs, i - 1)
}

type MyListR[T] = Int => T

def myEmptyList[T]: MyListR[T] = _ => sys.error("index out of range")
def myCons[T](x: T,  xs: MyListR[T]): MyListR[T] =
  i => if (i == 0) then x else xs(i - 1)

def length[T](l: MyList[T]): Int = l match {
  case MyEmptyList() => 0
  case MyCons(x, xs) => 1 + length(xs)
}

def allNats: MyListR[Int] = i => i

trait MyListRO[T] {
  def nth(i: Int): T
  def length: Int
}

def myEmptyListO[T]: MyListRO[T] = new MyListRO[T] {
  def nth(i: Int): T = sys.error("index out of range")
  def length: Int = 0
}

def myConsO[T](x: T,  xs: MyListRO[T]): MyListRO[T] = new MyListRO[T] {
  def nth(i: Int): T = if (i == 0) then x else xs.nth(i - 1)
  def length: Int =  1 + xs.length
}

