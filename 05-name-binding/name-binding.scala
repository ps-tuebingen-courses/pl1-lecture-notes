import scala.language.implicitConversions

object Syntax {
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
    case Mul(lhs: Exp, rhs: Exp) extends Exp
    case Id(x: String) extends Exp
    case With(x: String, xdef: Exp, body: Exp) extends Exp
}
import Syntax._
import Exp._

implicit def num2exp(n: Int): Exp = Num(n)
implicit def string2exp(x: String): Exp = Id(x)

val test = With("x", 5, Add("x", "x"))

def makeEval(subst: (Exp, String, Num) => Exp): Exp => Int = {
  def eval(e: Exp): Int = e match {
    case Num(n) => n
    case Id(x) => sys.error("unbound variable: " + x)
    case Add(l, r) => eval(l) + eval(r)
    case Mul(l, r) => eval(l) * eval(r)
    // take the Int and wrap it into a Num for substitution
    case With(x, xdef, body) => eval(subst(body, x, Num(eval(xdef))))
  }
  eval
}

val subst2: (Exp, String, Num) => Exp = (e, i, v) => e match {
  case Num(n) => e

  // Bound or free instance => substitute if names match
  case Id(x) => if (x == i) v else e

  case Add(l, r) => Add(subst2(l, i, v), subst2(r, i, v))
  case Mul(l, r) => Mul(subst2(l, i, v), subst2(r, i, v))

  // binding instance => do not substitute
  case With(x, xdef, body) => With(x,
                                   subst2(xdef, i, v),
                                   subst2(body, i, v))
}

def eval2 = makeEval(subst2)

assert(eval2(test) == 10) // it works!

val test2 = With("x", 5, Add("x", With("x", 3, 10))) // another test

assert(eval2(test2) == 15) // works as expected

val test3 = With("x", 5, Add("x", With("x", 3, "x"))) // another test

// assert(eval2(test3) == 8) // Bang! Result is 10 instead!

val subst3: (Exp, String, Num) => Exp = (e, i, v) => e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l, r) => Add(subst3(l, i, v), subst3(r, i, v))
    case Mul(l, r) => Mul(subst3(l, i, v), subst3(r, i, v))
    case With(x, xdef, body) => With(x,
                                     subst3(xdef, i, v),
                                     // what if we forget to substitute into the body?
                                     body)
}

def eval3 = makeEval(subst3)

assert(eval3(test) == 10)

assert(eval3(test2) == 15)

assert(eval3(test3) == 8) // Success!

val test4 = With("x", 5, Add("x", With("y", 3, "x")))

// assert(eval3(test4) == 10) // Bang! unbound variable: "x"
The inner expression should result in an error, because `x` has no value. Once again, substitution has changed a correct program into
val subst4: (Exp, String, Num) => Exp = (e, i, v) => e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l, r) => Add(subst4(l, i, v), subst4(r, i, v))
    case Mul(l, r) => Mul(subst4(l, i, v), subst4(r, i, v))
    // do not substitute when shadowed
    case With(x, xdef, body) => if (x == i) e
                                 else With(x,
                                           subst4(xdef, i, v),
                                           subst4(body, i, v))
}

def eval4 = makeEval(subst4)

assert(eval4(test) == 10)

assert(eval4(test2) == 15)

assert(eval4(test3) == 8)

assert(eval4(test4) == 10) // Success!

val test5 = With("x", 5, With("x", "x", "x"))

// assert(eval4(test5) == 5) // Bang! unbound variable "x"

val subst5: (Exp, String, Num) => Exp = (e, i, v) => e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l, r) => Add(subst5(l, i, v), subst5(r, i, v))
    case Mul(l, r) => Mul(subst5(l, i, v), subst5(r, i, v))
    // handle shadowing correctly
    case With(x, xdef, body) => With(x,
                                     subst5(xdef, i, v),
                                     if (x == i) body else subst5(body, i, v))
}

def eval5 = makeEval(subst5)

assert(eval5(test) == 10)

assert(eval5(test2) == 15)

assert(eval5(test3) == 8)

assert(eval5(test4) == 10)

assert(eval5(test5) == 5) // Success!

