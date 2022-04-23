object Syntax {
  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Id(name: String) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)

  // Both function definitions and applications are expressions.
  case class Fun(param: String, body: Exp) extends Exp
  case class App (funExpr: Exp, argExpr: Exp) extends Exp

  // "with" would be a better name for this function, but it is reserved in Scala
  def wth(x: String, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)
}

import Syntax._

def subst0(e1 : Exp, x: String, e2: Exp) : Exp = e1 match {
  case Num(n) => e1
  case Add(l,r) => Add(subst0(l,x,e2), subst0(r,x,e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case App(f,a) => App(subst0(f,x,e2), subst0(a,x,e2))
  case Fun(param,body) =>
    if (param == x) e1  else Fun(param, subst0(body, x, e2))
}

assert( subst0(Add(5,"x"), "x", 7) == Add(5, 7))
assert( subst0(Add(5,"x"), "y", 7) == Add(5,"x"))
assert( subst0(Fun("x", Add("x","y")), "x", 7) == Fun("x", Add("x","y")))

val subst0Test = subst0(Fun("x", Add("x","y")), "y", Add("x",5))

def freshName(names: Set[String], default: String) : String = {
  var last : Int = 0
  var freshName = default
  while (names contains freshName) { freshName = default+last; last += 1; }
  freshName
}

val freshNameExa1 = freshName(Set("y","z"),"x")
val freshNameExa2 = freshName(Set("x2","x0","x4","x","x1"),"x")
assert( freshNameExa1 == "x")
assert( freshNameExa2 == "x3")

def freeVars(e: Exp) : Set[String] =  e match {
   case Id(x) => Set(x)
   case Add(l,r) => freeVars(l) ++ freeVars(r)
   case Fun(x,body) => freeVars(body) - x
   case App(f,a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
}

val freeVarsExa = freeVars(Fun("x",Add("x","y")))
assert(freeVarsExa == Set("y"))

def subst(e1 : Exp, x: String, e2: Exp) : Exp = e1 match {
  case Num(n) => e1
  case Add(l,r) => Add(subst(l,x,e2), subst(r,x,e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case App(f,a) => App(subst(f,x,e2),subst(a,x,e2))
  case Fun(param,body) =>
    if (param == x) e1 else {
      val fvs = freeVars(Fun(param,body)) ++ freeVars(e2) + x
      val newvar = freshName(fvs, param)
      Fun(newvar, subst(subst(body, param, Id(newvar)), x, e2))
    }
}

assert( subst(Add(5,"x"), "x", 7) == Add(5, 7))
assert( subst(Add(5,"x"), "y", 7) == Add(5,"x"))
assert( subst(Fun("x", Add("x","y")), "x", 7) == Fun("x", Add("x","y")))
// test capture-avoiding substitution
assert( subst(Fun("x", Add("x","y")), "y", Add("x",5)) == Fun("x0",Add(Id("x0"),Add(Id("x"),Num(5)))))
assert( subst(Fun("x", Add(Id("x0"), Id("y"))), "y", Add(Id("x"), 5)) == Fun("x1", Add(Id("x0"), Add(Id("x"), Num(5)))) )

def eval(e: Exp) : Exp = e match {
  case Id(v) => sys.error("unbound identifier: " + v)
  case Add(l,r) => (eval(l), eval(r)) match {
                     case (Num(x),Num(y)) => Num(x+y)
                     case _ => sys.error("can only add numbers")
                    }
  case App(f,a) => eval(f) match {
     case Fun(x,body) => eval( subst(body,x, eval(a)))  // call-by-value
     // case Fun(x,body) => eval( subst(body,x, a))        // call-by-name
     case _ => sys.error("can only apply functions")
  }
  case _ => e // numbers and functions evaluate to themselves
}

def eval2(e: Exp) : Either[Num,Fun] = e match {
  case Id(v) => sys.error("unbound identifier: " + v)
  case Add(l,r) => (eval2(l), eval2(r)) match {
                     case (Left(Num(x)),Left(Num(y))) => Left(Num(x+y))
                     case _ => sys.error("can only add numbers")
                    }
  case App(f,a) => eval2(f) match {
     case Right(Fun(x,body)) => eval2( subst(body,x, eval(a)))
     case _ => sys.error("can only apply functions")
  }
  case f@Fun(_,_) => Right(f)
  case n@Num(_) => Left(n)
}

val test = App( Fun("x",Add("x",5)), 7)
assert( eval(test) == Num(12))

val omega = App(Fun("x",App("x","x")), Fun("x",App("x","x")))
// try eval(omega) to crash the interpreter ;-)

type Env0 = Map[String, Exp]

def evalWithEnv0(e: Exp, env: Env0) : Exp = e match {
  case Id(x) => env(x)
  case Add(l,r) => {
    (evalWithEnv0(l,env), evalWithEnv0(r,env)) match {
      case (Num(v1),Num(v2)) => Num(v1+v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case App(f,a) => evalWithEnv0(f,env) match {
    case Fun(x,body) => evalWithEnv0(body, Map(x -> evalWithEnv0(a,env)))
    case _ => sys.error("can only apply functions")
  }
  case _ => e // numbers and functions evaluate to themselves
}

assert( evalWithEnv0(test, Map.empty) == Num(12))

val test2 = wth("x", 5, App(Fun("f", App("f",3)), Fun("y",Add("x","y"))))

val evalTest2 = eval(test2)
assert(evalTest2 == Num(8))

val evalEnv0Test2 = evalWithEnv0(test2,Map.empty)

sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

def evalWithEnv(e: Exp, env: Env) : Value = e match {
  case Num(n: Int) => NumV(n)
  case Id(x) => env(x)
  case Add(l,r) => {
    (evalWithEnv(l,env), evalWithEnv(r,env)) match {
      case (NumV(v1),NumV(v2)) => NumV(v1+v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case f@Fun(param,body) => ClosureV(f, env)
  case App(f,a) => evalWithEnv(f,env) match {
    // Use environment stored in closure to realize proper lexical scoping!
    case ClosureV(f,closureEnv) => evalWithEnv(f.body, closureEnv + (f.param -> evalWithEnv(a,env)))
    case _ => sys.error("can only apply functions")
  }
}
assert( evalWithEnv(test, Map.empty) == NumV(12))
assert( evalWithEnv(test2,Map.empty) == NumV(8))
