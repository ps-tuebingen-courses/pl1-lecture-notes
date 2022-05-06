enum Exp:
  case Id(name: String)
  case Fun(param: String, body: Exp)
  case Ap (funExpr: Exp, argExpr: Exp)
  case PrintDot()

object Exp:
  implicit def id2exp(s: String): Exp = Id(s)

import Exp._

abstract class Value // the only values are closures
type Env = Map[String, Value]
case class ClosureV(f:Fun, env:Env) extends Value

def eval(e: Exp, env: Env) : Value = e match {
  // We give the identity function as dummy value for PrintDot
  case PrintDot() => print("."); ClosureV(Fun("x","x"), Map.empty)
  case Id(x) => env(x)
  case f@Fun(param,body) => ClosureV(f, env)
  case Ap(f,a) => eval(f,env) match {
    case ClosureV(f,closureEnv) => eval(f.body, closureEnv + (f.param -> eval(a,env)))
  }
}

val f = Fun("t", Fun("f", "f"))  // false
val t = Fun("t", Fun("f", "t"))  // true
val and = Fun("a", Fun("b", Ap(Ap("a", "b"),"a")))
val or = Fun("a", Fun("b", Ap(Ap("a", "a"), "b")))
val not = Fun("a", Fun("t", Fun("f", Ap(Ap("a","f"),"t"))))

val ifthenelse = Fun("cond", Fun("t", Fun("f", Ap(Ap("cond", "t"), "f"))))

val zero = Fun("s", Fun("z", "z"))
val succ = Fun("n", Fun("s", Fun("z", Ap("s", Ap(Ap("n", "s"),"z")))))
val one = Ap(succ, zero)
val two = Ap(succ, one)
val three = Ap(succ, two)
val add  = Fun("a", Fun("b", Fun("s", Fun("z", Ap(Ap("a","s"), Ap(Ap("b", "s"),"z"))))))
val mult = Fun("a", Fun("b", Fun("s", Fun("z", Ap(Ap("a", Ap("b","s")), "z")))))
val exp  = Fun("a", Fun("b", Ap(Ap("b", Ap(mult, "a")), one)))
val iszero = Fun("a", Ap(Ap("a", Fun("x", f)), t))

val printnum = Fun("a", Ap(Ap("a", Fun("x", PrintDot())), f))

val emptylist = Fun("c", Fun("e", "e"))
val cons = Fun("h", Fun("r", Fun("c", Fun("e", Ap(Ap("c", "h"), Ap(Ap("r","c"),"e"))))))
val head = Fun("l", Ap(Ap("l", Fun("h", Fun("t", "h"))), f))

val multlist = Fun("l", Ap(Ap("l", mult), one))

val list323 = Ap(Ap(cons, three), Ap(Ap(cons, two), Ap(Ap(cons,three),emptylist)))


val test = Ap(printnum, Ap(multlist, list323))

val exec = eval(test, Map.empty)

