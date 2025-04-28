import scala.language.implicitConversions

object Syntax {
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
    case Mul(lhs: Exp, rhs: Exp) extends Exp
    case Id(x: String) extends Exp
    case With(x: String, xdef: Exp, body: Exp) extends Exp

    /** The new language constructs for first-order functions: */
    case Call(f: String, args: List[Exp]) extends Exp // functions are called by name

  object Exp:
    /** The new language constructs for first-order functions: */
    case class FunDef(args: List[String], body: Exp)
    type Funs = Map[String, FunDef]

    /** We use implicits again to make example programs less verbose. */
    implicit def num2exp(n: Int): Exp = Num(n)
    implicit def string2exp(x: String): Exp = Id(x)
}
import Syntax._
import Exp._

def subst(e: Exp, i: String, v: Num): Exp =  e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l, r) => Add(subst(l, i, v), subst(r, i, v))
    case Mul(l, r) => Mul(subst(l, i, v), subst(r, i, v))
    case With(x, xdef, body) => With(x,
                                     subst(xdef, i, v),
                                     if (x == i) body else subst(body, i, v))
    case Call(f, args) => Call(f, args.map(subst(_, i, v)))
}

def eval(funs: Funs, e: Exp): Int = e match {
  case Num(n) => n
  case Id(x) => sys.error("unbound identifier: " + x)
  case Add(l, r) => eval(funs, l) + eval(funs, r)
  case Mul(l, r) => eval(funs, l) * eval(funs, r)
  case With(x, xdef, body) => eval(funs, subst(body, x, Num(eval(funs, xdef))))
  case Call(f, args) => {
     val fd = funs(f) // lookup function definition
     val vargs = args.map(eval(funs, _)) // evaluate function arguments
     if (fd.args.size != vargs.size)
       sys.error("number of parameters in call to " + f + " does not match")
     // We construct the function body to be evaluated by subsequently substituting
     // all formal arguments with their respective argument values.
     // If we have only a single argument "fd.arg" and a single argument value "varg",
     // the next line of code is equivalent to:
     // val substbody = subst(fd.body, fd.arg, Num(varg))
     val substbody = fd.args.zip(vargs).foldLeft(fd.body)((b, av) => subst(b, av._1, Num(av._2)))
     eval(funs, substbody)
  }
}

val someFuns: Funs = Map("adder" -> FunDef(List("a", "b"), Add("a", "b")),
                         "doubleadder" -> FunDef(List("a", "x"),
                                                 Add(Call("adder", List("a", 5)),
                                                     Call("adder", List("x", 7)))))

val callSomeFuns = eval(someFuns, Call("doubleadder", List(2, 3)))
assert(callSomeFuns == 17)

val testProg = With("x", 1, With("y", 2, With("z", 3, Add("x", Add("y", "z")))))

val testProgAfterOneStep     = With("y", 2, With("z", 3, Add(1, Add("y", "z"))))
val testProgAfterTwoSteps    = With("z", 3, Add(1, Add(2, "z")))
val testProgAfterThreeSteps  = Add(1, Add(2, 3))

type Env = Map[String, Int]

def evalWithEnv(funs: Funs, env: Env, e: Exp): Int = e match {
  case Num(n) => n
  case Id(x) => env(x) // look up in repository of deferred substitutions
  case Add(l, r) => evalWithEnv(funs, env, l) + evalWithEnv(funs, env, r)
  case Mul(l, r) => evalWithEnv(funs, env, l) * evalWithEnv(funs, env, r)
  case With(x, xdef, body) => evalWithEnv(funs, env + ((x, evalWithEnv(funs, env, xdef))), body)
  case Call(f, args) => {
     val fd = funs(f) // lookup function definition
     val vargs = args.map(evalWithEnv(funs, env, _)) // evaluate function arguments
     if (fd.args.size != vargs.size)
       sys.error("number of parameters in call to " + f + " does not match")
     // We construct the environment by associating each formal argument to its actual value
     val newenv = Map() ++ fd.args.zip(vargs)
     evalWithEnv(funs, newenv, fd.body)
  }
}

val evalEnvSomeFuns = evalWithEnv(someFuns, Map.empty, Call("doubleadder", List(2, 3)))
assert(evalEnvSomeFuns == 17)

def evalDynScope(funs: Funs, env: Env, e: Exp): Int = e match {
  case Num(n) => n
  case Id(x) => env(x)
  case Add(l, r) => evalDynScope(funs, env, l) + evalDynScope(funs, env, r)
  case Mul(l, r) => evalDynScope(funs, env, l) * evalDynScope(funs, env, r)
  case With(x, xdef, body) => evalDynScope(funs, env + ((x, evalDynScope(funs, env, xdef))), body)
  case Call(f, args) => {
     val fd = funs(f)
     val vargs = args.map(evalDynScope(funs, env, _))
     if (fd.args.size != vargs.size)
       sys.error("number of parameters in call to " + f + " does not match")
     val newenv = env ++ fd.args.zip(vargs) // extending env instead of Map() !!
     evalDynScope(funs, newenv, fd.body)
  }
}

val evalDynSomeFuns = evalDynScope(someFuns, Map.empty, Call("doubleadder", List(2, 3)))
assert(evalDynSomeFuns == 17)

val funnyFun: Funs = Map("funny" -> FunDef(List("a"), Add("a", "b")))

val evalDynFunnyFun = evalDynScope(funnyFun, Map.empty, With("b", 3, Call("funny", List(4))))
assert(evalDynFunnyFun == 7)

