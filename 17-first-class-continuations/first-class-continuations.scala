import scala.language.implicitConversions

enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp)
  case Ap (funExpr: Exp, argExpr: Exp)
  /** The abstract syntax of Letcc is as follows */
  case Letcc(param: String, body: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)

import Exp._

sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

case class ContV(f: Value => Nothing) extends Value

def eval(e: Exp, env: Env, k: Value => Nothing) : Nothing = e match {
  case Num(n: Int) => k(NumV(n))
  case Id(x) => k(env(x))
  case Add(l,r) => {
    eval(l,env, lv =>
        eval(r,env, rv =>
          (lv,rv) match {
            case (NumV(v1), NumV(v2)) => k(NumV(v1+v2))
            case _ => sys.error("can only add numbers")
          }))
  }
  case f@Fun(param,body) => k(ClosureV(f, env))

  /* In the application case we now need to distinguish whether the first argument
   * is a closure or a continuation. If it is a continuation, we ignore the
   * current continuation k and "jump" to the stored continuation by applying the
   * evaluated continuation argument to it. */
  case Ap(f,a) => eval(f,env, cl => cl match {
            case ClosureV(f,closureEnv) => eval(a,env, av => eval(f.body, closureEnv + (f.param -> av),k))
            case ContV(f) => eval(a,env, av => f(av))
            case _ => sys.error("can only apply functions")
  })
  /* Letcc is now surprisingly simple: We continue the evaluation in the body in an
   * extended environment in which param is bound to the current continuation k,
   * wrapped as a value using ContV. */
  case Letcc(param,body) => eval(body, env+(param -> ContV(k)), k)
}

def starteval(e: Exp) : Value = {
  var res : Value = null
  val s : Value => Nothing = (v) => { res = v; sys.error("program terminated") }
  try { eval(e, Map.empty, s) } catch { case e:Throwable => () }
  res
}

val testprog = Add(1, Letcc("k", Add(2, Ap("k", 3))))

assert(starteval(testprog) == NumV(4))
