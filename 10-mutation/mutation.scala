import scala.language.implicitConversions

object Syntax {
  enum Exp:
    case Num(n: Int)
    case Id(name: String)
    case Add(lhs: Exp, rhs: Exp)
    case Mul(lhs: Exp, rhs: Exp)
    case If0(cond: Exp, thenExp: Exp, elseExp: Exp)
    case Fun(param: String, body: Exp)
    case Ap(funExpr: Exp, argExpr: Exp)

    /** To add mutation to FAE, we add four language constructs: */
    case NewBox(e: Exp) // create a new box
    case SetBox(b: Exp, e: Exp) // assign to a box
    case OpenBox(b: Exp) // read value in a box
    case Seq(e1: Exp, e2: Exp) // sequencing of expressions

  object Exp:
    implicit def num2exp(n: Int): Exp = Num(n)
    implicit def id2exp(s: String): Exp = Id(s)
    def wth(x: String, xdef: Exp, body: Exp): Exp = Ap(Fun(x, body), xdef)
}
import Syntax._
import Exp._

val test1 = wth("b", NewBox(0),
              Seq(
                SetBox("b", Add(1, OpenBox("b"))),
                OpenBox("b")))

val test2 = wth("a", NewBox(1),
              wth("f", Fun("x", Add("x", OpenBox("a"))),
                Seq(SetBox("a", 2),
                  Ap("f", 5))))

sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

type Address = Int
case class AddressV(a: Address) extends Value

type Store = Map[Address, Value]

var _nextAddress = 0

def nextAddress: Address = {
  _nextAddress += 1
  _nextAddress
}

val test3 = wth("switch", NewBox(0),
             wth("toggle", Fun("dummy", If0(OpenBox("switch"),
                                          Seq(SetBox("switch", 1), 1),
                                          Seq(SetBox("switch", 0), 0))),
                 Add(Ap("toggle", 42), Ap("toggle", 42))))

def eval(e: Exp, env: Env, s: Store): (Value, Store) = e match {
  /* All expressions whose evaluation does not alter the store just return s. */
  case Num(n) => (NumV(n), s)
  case Id(x) => (env(x), s)
  case f@Fun(_, _) => (ClosureV(f, env), s)


// In recursive cases we have to thread the store through the evaluation. In particular, we define the order of evaluation
// explicitly through data flow dependencies.

  case If0(cond, thenExp, elseExp)
    => eval(cond, env, s) match {
         case (NumV(0), s1) => eval(thenExp, env, s1)
         case (_, s1)       => eval(elseExp, env, s1)

         /* An alternative that enfoces runtime type-correctness of
          * the conditional expression:
         case (NumV(_), s1) => eval(elseExp, env, s1)
         case _             => sys.error("can only test if a number is 0") */
       }

  case Add(l, r)
    => eval(l, env, s) match {
         case (NumV(v1), s1)
           => eval(r, env, s1) match {
                case (NumV(v2), s2) => (NumV(v1 + v2), s2)
                case _ => sys.error("can only add numbers")
              }
         case _
           => sys.error("can only add numbers")
       }

  case Mul(l, r)
    => eval(l, env, s) match {
         case (NumV(v1), s1)
           => eval(r, env, s1) match {
                case (NumV(v2), s2) => (NumV(v1 * v2), s2)
                case _ => sys.error("can only multiply numbers")
              }
         case _ => sys.error("can only multiply numbers")
       }

  case Ap(f, a)
    => eval(f, env, s) match {
         case (ClosureV(f, closureEnv), s1)
           => eval(a, env, s1) match {
                case (av, s2)
                  => eval(f.body, closureEnv + (f.param -> av), s2)
              }
         case _ => sys.error("can only apply functions")
       }


// In a sequence, we ignore the result of evaluating e1 but not its effect on the store.

  case Seq(e1, e2) => eval(e2, env, eval(e1, env, s)._2)

//  A new box is created by putting it into the store at a new address.

  case NewBox(e: Exp)
    => eval(e, env, s) match {
         case (v, s1) => {
           val a = nextAddress
           (AddressV(a), s1 + (a -> v))
         }
       }

// Setting a box is now a two-step process: First evaluate b to an
// address, then lookup and update the value associated to the
// address in the store. Note that "updated" is a functional method.

  case SetBox(b: Exp, e: Exp)
    => eval(b, env, s) match {
         case (AddressV(a), s1)
           => eval(e, env, s1) match {
                case (ev, s2) => (ev, s2.updated(a, ev))
              }
         case _ => sys.error("can only set boxes")
       }

// OpenBox uses the same two-step process but does not update the
// store.

  case OpenBox(b: Exp)
    => eval(b, env, s) match {
         case (AddressV(a), s1) => (s1(a), s1)
         case _                 => sys.error("can only open boxes")
       }
}

def gc(env: Env, store: Store): Store = {

  def allAddrInVal(v: Value): Set[Address] = v match {
    case AddressV(a)      => Set(a)
    case NumV(_)          => Set.empty
    case ClosureV(f, env) => allAddrInEnv(env)
  }

  def allAddrInEnv(env: Env): Set[Address] =
    env.values.map(allAddrInVal _).fold(Set.empty)(_ union _)

  def mark(seed: Set[Address]): Set[Address] = {
    val newAddresses = seed.flatMap(ad => allAddrInVal(store(ad)))
    if (newAddresses.subsetOf(seed)) seed
    else mark(seed union newAddresses)
  }

  val marked = mark(allAddrInEnv(env)) // mark ...
  store.view.filterKeys(marked(_)).toMap // and sweep!
}

val teststore = Map(
  6  -> NumV(42),
  7  -> NumV(6),
  8  -> AddressV(6),
  9  -> AddressV(7),
  10 -> ClosureV(Fun("x", "y"), Map("y" -> AddressV(8)))
)

/*
10 -> 8 -> 6
      9 -> 7        */

assert(gc(Map("a" -> AddressV(10)), teststore) == teststore - 7 - 9)

