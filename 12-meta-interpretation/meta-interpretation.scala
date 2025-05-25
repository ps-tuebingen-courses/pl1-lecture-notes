object HOAS {
  enum Exp:
    case Num(n: Int)
    case Id(name: String)
    case Add(lhs: Exp, rhs: Exp)
    case Fun(f: Exp => Exp)
    case Ap(funExpr: Exp, argExpr: Exp)

  import Exp._

  def eval(e: Exp): Exp = e match {
      case Id(v) => sys.error("unbound identifier: " + v)
      case Add(l, r) => (eval(l), eval(r)) match {
                         case (Num(x), Num(y)) => Num(x + y)
                         case _ => sys.error("can only add numbers")
                        }
      case Ap(f, a) => eval(f) match {
         case Fun(f) => eval(f(eval(a)))
         case _ => sys.error("can only apply functions")
      }
      case _ => e // numbers and functions evaluate to themselves
    }
}

enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp)
  case Ap(funExpr: Exp, argExpr: Exp)

import Exp._

object Compositional {
    sealed abstract class Value
    type Env = Map[String, Value]
    case class NumV(n: Int) extends Value
    case class FunV(f: Value => Value) extends Value

    def eval(e: Exp): Env => Value = e match {
      case Num(n: Int) => env => NumV(n)
      case Id(x) => env => env(x)
      case Add(l, r) => { env =>
        (eval(l)(env), eval(r)(env)) match {
          case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
          case _ => sys.error("can only add numbers")
        }
      }
      case Fun(param, body) => env => FunV((v) => eval(body)(env + (param -> v)))
      case Ap(f, a) => env => (eval(f)(env), eval(a)(env)) match {
        // Use environment stored in (meta-level) closure to realize proper lexical scoping!
        case (FunV(g), arg) => g(arg)
        case _ => sys.error("can only apply functions")
      }
    }
}

object FAE {
    sealed abstract class Value
    type Env = Map[String, Value]
    case class NumV(n: Int) extends Value
    case class ClosureV(f: Fun, env: Env) extends Value

    def eval(e: Exp, env: Env): Value = e match {
      case Num(n: Int) => NumV(n)
      case Id(x) => env(x)
      case Add(l, r) => {
        (eval(l, env), eval(r, env)) match {
          case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
          case _ => sys.error("can only add numbers")
        }
      }
      case f@Fun(param, body) => ClosureV(f, env)
      case Ap(f, a) => eval(f, env) match {
        // Use environment stored in closure to realize proper lexical scoping!
        case ClosureV(f, closureEnv) => eval(f.body, closureEnv + (f.param -> eval(a, env)))
        case _ => sys.error("can only apply functions")
      }
    }
}

