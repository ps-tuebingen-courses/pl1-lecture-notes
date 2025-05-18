object Syntax {
  enum Exp:
    case Num(n: Int)
    case Id(name: String)
    case Add(lhs: Exp, rhs: Exp)

    // Both function definitions and applications are expressions.
    case Fun(param: String, body: Exp)
    case Ap(funExpr: Exp, argExpr: Exp)

  object Exp:
    implicit def num2exp(n: Int): Exp = Num(n)
    implicit def id2exp(s: String): Exp = Id(s)
    // "with" would be a better name for this function, but it is reserved in Scala
    def wth(x: String, xdef: Exp, body: Exp): Exp = Ap(Fun(x, body), xdef)
}

import Syntax._
import Exp._

def freshName(names: Set[String], default: String): String = {
  var last: Int = 0
  var freshName = default
  while (names contains freshName) { freshName = default + last; last += 1; }
  freshName
}

def freeVars(e: Exp): Set[String] =  e match {
   case Id(x) => Set(x)
   case Add(l, r) => freeVars(l) ++ freeVars(r)
   case Fun(x, body) => freeVars(body) - x
   case Ap(f, a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
}

def subst(e1: Exp, x: String, e2: Exp): Exp = e1 match {
  case Num(n) => e1
  case Add(l, r) => Add(subst(l, x, e2), subst(r, x, e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case Ap(f, a) => Ap(subst(f, x, e2), subst(a, x, e2))
  case Fun(param, body) =>
    if (param == x) e1 else {
      val fvs = freeVars(Fun(param, body)) ++ freeVars(e2) + x
      val newvar = freshName(fvs, param)
      Fun(newvar, subst(subst(body, param, Id(newvar)), x, e2))
    }
}

def eval(e: Exp): Exp = e match {
  case Id(v) => sys.error("unbound identifier: " + v)
  case Add(l, r) => (eval(l), eval(r)) match {
                     case (Num(x), Num(y)) => Num(x + y)
                     case _ => sys.error("can only add numbers")
                    }
  case Ap(f, a) => eval(f) match {
     case Fun(x, body) => eval(subst(body, x, eval(a)))  // call-by-value
     // case Fun(x, body) => eval(subst(body, x, a))        // call-by-name
     case _ => sys.error("can only apply functions")
  }
  case _ => e // numbers and functions evaluate to themselves
}

def eval2(e: Exp): Either[Num, Fun] = e match {
  case Id(v) => sys.error("unbound identifier: " + v)
  case Add(l, r) => (eval2(l), eval2(r)) match {
                     case (Left(Num(x)), Left(Num(y))) => Left(Num(x + y))
                     case _ => sys.error("can only add numbers")
                    }
  case Ap(f, a) => eval2(f) match {
     case Right(Fun(x, body)) => eval2(subst(body, x, eval(a)))
     case _ => sys.error("can only apply functions")
  }
  case f@Fun(_, _) => Right(f)
  case n@Num(_) => Left(n)
}

val test = Ap(Fun("x", Add("x", 5)), 7)

val omega = Ap(Fun("x", Ap("x", "x")), Fun("x", Ap("x", "x")))
// try eval(omega) to crash the interpreter ;-)

val test2 = wth("x", 5, Ap(Fun("f", Ap("f", 3)), Fun("y", Add("x", "y"))))

sealed abstract class Value
type Env = Map[String, Value]

case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

def evalWithEnv(e: Exp, env: Env): Value = e match {
  case Num(n: Int) => NumV(n)
  case Id(x) => env(x)
  case Add(l, r) => {
    (evalWithEnv(l, env), evalWithEnv(r, env)) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case f@Fun(param, body) => ClosureV(f, env)
  case Ap(f, a) => evalWithEnv(f, env) match {
    // Use environment stored in closure to realize proper lexical scoping!
    case ClosureV(f, closureEnv) => evalWithEnv(f.body, closureEnv + (f.param -> evalWithEnv(a, env)))
    case _ => sys.error("can only apply functions")
  }
}

def evalcbn(e: Exp): Exp = e match {
  case Id(v) => sys.error("unbound identifier: " + v)
  case Add(l, r) => (evalcbn(l), evalcbn(r)) match {
                     case (Num(x), Num(y)) => Num(x + y)
                     case _ => sys.error("can only add numbers")
                    }
  case Ap(f, a) => evalcbn(f) match {
     case Fun(x, body) => evalcbn(subst(body, x, a)) // no evaluation of a!
     case _ => sys.error("can only apply functions")
  }
  case _ => e
}

assert(evalcbn(test) == eval(test))
assert(evalcbn(test2) == eval(test2))

val test3 = Ap(Fun("a", Fun("x", Add("a", "x"))), Add(1, 2))

assert(eval(test3) == Fun("x", Add(3, "x")))
assert(evalcbn(test3) == Fun("x", Add(Add(1, 2), "x")))

 val test4 = Ap(Fun("x", 5), omega)

assert(evalcbn(test4) == Num(5))

val nil = Fun("c", Fun("e", "e"))
val cons = Fun("x", Fun("xs",
             Fun("c", Fun("e", Ap(Ap("c", "x"), Ap(Ap("xs", "c"), "e"))))))

val list123 = Ap(Ap("cons", 1), Ap(Ap("cons", 2), Ap(Ap("cons", 3), "nil")))

val maplist = Fun("f", Fun("l",
                Ap(Ap("l",
                   Fun("x", Fun("xs", Ap(Ap("cons", Ap("f", "x")), "xs")))),
                   "nil")))

val test5 = wth("cons", cons,
            wth("nil", nil,
            wth("maplist", maplist,
            Ap(Ap("maplist", Fun("x", Add("x", 1))), list123))))

val test5res = wth("cons", cons,
               wth("nil", nil,
                 Ap(Ap("cons", 2), Ap(Ap("cons", 3), Ap(Ap("cons", 4), "nil")))))
assert(eval(test5) == eval(test5res))

val y = Fun("f", Ap(Fun("x", Ap("f", Ap("x", "x"))), Fun("x", Ap("f", Ap("x", "x")))))

val allnats = Ap(Ap("y",
                 Fun("nats", Fun("n", Ap(Ap("cons", "n"), Ap("nats", Add("n", 1)))))),
              1)

val list2toinfty = wth("cons", cons,
                   wth("nil", nil,
                   wth("y", y,
                   wth("maplist", maplist,
                      Ap(Ap("maplist", Fun("x", Add("x", 1))), allnats)))))

trait CBN {
  type Thunk

  def delay(e: Exp, env: EnvThunk): Thunk
  def force(t: Thunk): ValueCBN

  case class EnvThunk(map: Map[String, Thunk]) {
    def apply(key: String): Thunk = map.apply(key)
    def +(other: (String, Thunk)): EnvThunk = EnvThunk(map + other)
  }

  // since values also depend on EnvThunk and hence on Thunk they need to
  // be defined within this trait
  sealed abstract class ValueCBN
  case class NumV(n: Int) extends ValueCBN
  case class ClosureV(f: Fun, env: EnvThunk) extends ValueCBN
  def evalCBN(e: Exp, env: EnvThunk): ValueCBN = e match {
    case Id(x) => force(env(x)) // force evaluation of thunk if identifier is evaluated
    case Add(l, r) => {
      (evalCBN(l, env), evalCBN(r, env)) match {
        case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
        case _ => sys.error("can only add numbers")
      }
    }
    case Ap(f, a) => evalCBN(f, env) match {
      // delay argument expression and add it to environment of the closure
      case ClosureV(f, cenv) => evalCBN(f.body, cenv + (f.param -> delay(a, env)))
      case _ => sys.error("can only apply functions")
    }
    case Num(n) => NumV(n)
    case f@Fun(x, body) => ClosureV(f, env)
  }
}

object CallByName extends CBN {
  type Thunk = (Exp, EnvThunk)
  def delay(e: Exp, env: EnvThunk) = (e, env)
  def force(t: Thunk) = {
    println("Forcing evaluation of expression: " + t._1)
    evalCBN(t._1, t._2)
  }
}

assert(CallByName.evalCBN(test, CallByName.EnvThunk(Map.empty)) == CallByName.NumV(12))

val cbntest = wth("double", Fun("x", Add("x", "x")),
              Ap("double", Add(2, 3)))

val blowup = wth("a", Fun("x", Add("x", "x")),
             wth("b", Fun("x", Add(Ap("a", "x"), Ap("a", "x"))),
             wth("c", Fun("x", Add(Ap("b", "x"), Ap("b", "x"))),
             wth("d", Fun("x", Add(Ap("c", "x"), Ap("c", "x"))),
             wth("e", Fun("x", Add(Ap("d", "x"), Ap("d", "x"))),
             wth("f", Fun("x", Add(Ap("e", "x"), Ap("e", "x"))),
             Ap("f", Add(2, 3))))))))

object CallByNeed extends CBN {
  case class MemoThunk(e: Exp, env: EnvThunk) {
    var cache: ValueCBN = null
  }
  type Thunk = MemoThunk
  def delay(e: Exp, env: EnvThunk) = MemoThunk(e, env)
  def force(t: Thunk) = {
    if (t.cache == null) {
      println("Forcing evaluation of expression: " + t.e)
      t.cache = evalCBN(t.e, t.env)
    } else {
      println ("Reusing cached value " + t.cache + " for expression " + t.e)
    }
    t.cache
  }
}

