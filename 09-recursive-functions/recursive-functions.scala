import scala.language.implicitConversions

object Syntax {
  enum Exp:
    case Num(n: Int)
    case Id(name: String)
    case Add(lhs: Exp, rhs: Exp)
    case If0(cond: Exp, thenExp: Exp, elseExp: Exp)
    case Fun(param: String, body: Exp)
    case Ap(funExpr: Exp, argExpr: Exp)
    case Letrec(x: String, e: Exp, body: Exp)

  object Exp:
    implicit def num2exp(n: Int): Exp = Num(n)
    implicit def id2exp(s: String): Exp = Id(s)
    def wth(x: String, xdef: Exp, body: Exp): Exp = Ap(Fun(x, body), xdef)
}

import Syntax._
import Exp._

val sum = Letrec("sum", Fun("n", If0("n", 0, Add("n", Ap("sum", Add("n", -1))))), Ap("sum", 10))

object Values {
  trait ValueHolder {
    def value: Value
  }
  sealed abstract class Value extends ValueHolder { def value = this }
  case class ValuePointer(var v: Value) extends ValueHolder { def value = v }
  case class NumV(n: Int) extends Value
  case class ClosureV(f: Fun, env: Env) extends Value
  type Env = Map[String, ValueHolder]
}

import Values._  // such that we do not have to write Values.ValueHolder etc.

def eval(e: Exp, env: Env): Value = e match {
  case Num(n: Int) => NumV(n)
  case Id(x) => env(x).value  // dereference potential ValuePointer
  case If0(cond, thenExp, elseExp) => eval(cond, env) match {
    case NumV(0) => eval(thenExp, env)
    case _ => eval(elseExp, env)
  }
  case Add(l, r) => {
    (eval(l, env), eval(r, env)) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case f@Fun(param, body) => ClosureV(f, env)
  case Ap(f, a) => eval(f, env) match {
    case ClosureV(f, closureEnv) => eval(f.body, closureEnv + (f.param -> eval(a, env)))
    case _ => sys.error("can only apply functions")
  }
  case Letrec(x, e, body) => {
    val vp = ValuePointer(null)  // initialize pointer with null
    val newenv = env + (x -> vp)  // evaluate e in the environment extended with the placeholder
    vp.v = eval(e, newenv)         // create the circle in the environment
    eval(body, newenv) // evaluate body in circular environment
  }
}

assert(eval(sum, Map.empty) == NumV(55))

// These test cases were contributed by rzhxeo (Sebastian Py)
var func = Fun("n", If0("n", 0, Ap("func", Add("n", -1))))
var test1 = Letrec("func", func, Ap("func", 1))
var test2 = Letrec("func", Ap(Fun("notUsed", func), 0), Ap("func", 1))
assert(eval(test1, Map()) == NumV(0))
assert(eval(test2, Map()) == NumV(0))
