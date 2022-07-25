enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp)
  case Ap(funExpr: Exp, argExpr: Exp)
  case Shift(param: String, body: Exp)
  case Reset(body: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)

import Exp._

sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value
case class ContV(f: Value => Value) extends Value

def eval(e: Exp, env: Env, k: Value => Value): Value = e match {
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

  case Ap(f, a) => eval(f, env, cl => cl match {
            case ClosureV(f, closureEnv) => eval(a, env, av => eval(f.body, closureEnv + (f.param -> av), k))
            // compose continuations k2 and k
            case ContV(k2) => eval(a, env, av => k(k2(av)))
            case _ => sys.error("can only apply functions")
  })
  // reset the continuation to the identity function
  case Reset(e) => k(eval(e, env, x => x))
  // wrap current continuation and reset continuation
  case Shift(param, body) => eval(body, env + (param -> ContV(k)), x => x)
}

