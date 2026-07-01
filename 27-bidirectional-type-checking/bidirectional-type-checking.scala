import scala.language.implicitConversions

sealed abstract class Type

enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp)           // no type annotation (checked)
  case Ap(funExpr: Exp, argExpr: Exp)
  case Junit()
  case Let(x: String, xdef: Exp, body: Exp)
  case TypeAscription(e: Exp, t: Type)         // the bridge between the two modes

  case Product(e1: Exp, e2: Exp)
  case Fst(e: Exp)
  case Snd(e: Exp)

  case SumLeft(left: Exp)                       // no type annotation (checked)
  case SumRight(right: Exp)                     // no type annotation (checked)
  case EliminateSum(e: Exp, fl: Exp, fr: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)

import Exp._

def freshName(names: Set[String], default: String): String = {
  var last: Int = 0
  var freshName = default
  while (names contains freshName) { freshName = default + last.toString; last += 1; }
  freshName
}

def freeVars(e: Exp): Set[String] = e match {
  case Id(x) => Set(x)
  case Add(l, r) => freeVars(l) ++ freeVars(r)
  case Fun(x, body) => freeVars(body) - x
  case Ap(f, a) => freeVars(f) ++ freeVars(a)
  case Num(n) => Set.empty
  case Junit() => Set.empty
  case TypeAscription(e, t) => freeVars(e)
  case Let(x, xdef, body) => freeVars(xdef) ++ (freeVars(body) - x)
  case Product(e1, e2) => freeVars(e1) ++ freeVars(e2)
  case Fst(e) => freeVars(e)
  case Snd(e) => freeVars(e)
  case SumLeft(e) => freeVars(e)
  case SumRight(e) => freeVars(e)
  case EliminateSum(e, fl, fr) => freeVars(e) ++ freeVars(fl) ++ freeVars(fr)
}

def subst(e1: Exp, x: String, e2: Exp): Exp = e1 match {
  case Num(n) => e1
  case Junit() => e1
  case Add(l, r) => Add(subst(l, x, e2), subst(r, x, e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case Ap(f, a) => Ap(subst(f, x, e2), subst(a, x, e2))
  case TypeAscription(e, t) => TypeAscription(subst(e, x, e2), t)
  case Fun(param, body) =>
    if (param == x) e1 else {
      val fvs = freeVars(body) ++ freeVars(e2)
      val newvar = freshName(fvs, param)
      Fun(newvar, subst(subst(body, param, Id(newvar)), x, e2))
    }
  case Let(y, ydef, body) =>
    if (x == y) Let(y, subst(ydef, x, e2), body) else {
      val fvs = freeVars(body) ++ freeVars(e2)
      val newvar = freshName(fvs, y)
      Let(newvar, subst(ydef, x, e2), subst(subst(body, y, Id(newvar)), x, e2))
    }
  case Product(a, b) => Product(subst(a, x, e2), subst(b, x, e2))
  case Fst(e) => Fst(subst(e, x, e2))
  case Snd(e) => Snd(subst(e, x, e2))
  case SumLeft(e) => SumLeft(subst(e, x, e2))
  case SumRight(e) => SumRight(subst(e, x, e2))
  case EliminateSum(e, fl, fr) =>
    EliminateSum(subst(e, x, e2), subst(fl, x, e2), subst(fr, x, e2))
}

def eval(e: Exp): Exp = e match {
  case Id(v) => sys.error("unbound identifier: " + v)
  case Add(l, r) => (eval(l), eval(r)) match {
    case (Num(x), Num(y)) => Num(x + y)
    case _ => sys.error("can only add numbers")
  }
  case Ap(f, a) => eval(f) match {
    case Fun(x, body) => eval(subst(body, x, eval(a))) // call-by-value
    case _ => sys.error("can only apply functions")
  }
  case TypeAscription(e, _) => eval(e)
  case Let(x, xdef, body) => eval(subst(body, x, eval(xdef)))
  case Product(a, b) => Product(eval(a), eval(b))
  case Fst(e) => eval(e) match {
    case Product(a, b) => a
    case _ => sys.error("can only apply Fst to products")
  }
  case Snd(e) => eval(e) match {
    case Product(a, b) => b
    case _ => sys.error("can only apply Snd to products")
  }
  case SumLeft(e) => SumLeft(eval(e))
  case SumRight(e) => SumRight(eval(e))
  case EliminateSum(e, fl, fr) => eval(e) match {
    case SumLeft(e2)  => eval(Ap(fl, e2))
    case SumRight(e2) => eval(Ap(fr, e2))
    case _ => sys.error("can only eliminate sums")
  }
  case _ => e // numbers and functions evaluate to themselves
}

case class NumType() extends Type
case class FunType(from: Type, to: Type) extends Type
case class JunitType() extends Type
case class ProductType(fst: Type, snd: Type) extends Type
case class SumType(left: Type, right: Type) extends Type

def infer(e: Exp, gamma: Map[String, Type]): Type = e match {
  case Num(_)  => NumType()
  case Junit() => JunitType()
  case Id(x)   => gamma.getOrElse(x, sys.error("free variable: " + x))

  case Add(l, r) =>
    check(l, NumType(), gamma); check(r, NumType(), gamma); NumType()

  // Elimination form: synthesize the function's type, then CHECK the argument
  // against its domain. This is what lets an unannotated argument lambda work.
  case Ap(f, a) =>
    infer(f, gamma) match {
      case FunType(from, to) => check(a, from, gamma); to
      case t => sys.error("application of a non-function of type " + t)
    }

  // The bridge from checking to synthesis: the ascription supplies the type
  // against which a check-only term (e.g. a bare lambda) is checked.
  case TypeAscription(e, t) => check(e, t, gamma); t

  // Let synthesizes if its definition does; the binding's type is remembered.
  case Let(x, xdef, body) =>
    infer(body, gamma + (x -> infer(xdef, gamma)))

  // Products synthesize when both components synthesize.
  case Product(e1, e2) => ProductType(infer(e1, gamma), infer(e2, gamma))

  case Fst(e) => infer(e, gamma) match {
    case ProductType(a, _) => a
    case t => sys.error("Fst of a non-product of type " + t)
  }
  case Snd(e) => infer(e, gamma) match {
    case ProductType(_, b) => b
    case t => sys.error("Snd of a non-product of type " + t)
  }

  // The check-only forms have no synthesis rule: ask for an annotation.
  case Fun(_, _) =>
    sys.error("cannot synthesize the type of an unannotated function; add a type ascription")
  case SumLeft(_) =>
    sys.error("cannot synthesize the type of a Left injection; add a type ascription")
  case SumRight(_) =>
    sys.error("cannot synthesize the type of a Right injection; add a type ascription")
  case EliminateSum(_, _, _) =>
    sys.error("cannot synthesize the type of a case expression; add a type ascription")
}

def check(e: Exp, expected: Type, gamma: Map[String, Type]): Unit = e match {
  // Unannotated lambda: recover the parameter type from the expected function type.
  case Fun(x, body) => expected match {
    case FunType(from, to) => check(body, to, gamma + (x -> from))
    case t => sys.error("a function must be checked against a function type, but got " + t)
  }

  // Injections recover the "other" summand from the expected sum type.
  case SumLeft(l) => expected match {
    case SumType(left, _) => check(l, left, gamma)
    case t => sys.error("a Left injection must be checked against a sum type, but got " + t)
  }
  case SumRight(r) => expected match {
    case SumType(_, right) => check(r, right, gamma)
    case t => sys.error("a Right injection must be checked against a sum type, but got " + t)
  }

  // Case analysis: synthesize the scrutinee's sum type, then check each branch
  // as a function into the EXPECTED result type. Branches may be bare lambdas;
  // their parameter types come from the scrutinee.
  case EliminateSum(scrut, fl, fr) => infer(scrut, gamma) match {
    case SumType(left, right) =>
      check(fl, FunType(left, expected), gamma)
      check(fr, FunType(right, expected), gamma)
    case t => sys.error("can only eliminate sums, but the scrutinee has type " + t)
  }

  // Propagating the expected type into a pair lets us check a pair of lambdas.
  case Product(e1, e2) => expected match {
    case ProductType(a, b) => check(e1, a, gamma); check(e2, b, gamma)
    case _ => subsume(e, expected, gamma)
  }

  // Let can pass the expected type on to its body.
  case Let(x, xdef, body) =>
    check(body, expected, gamma + (x -> infer(xdef, gamma)))

  // Subsumption: everything else synthesizes; infer and compare.
  case _ => subsume(e, expected, gamma)
}

def subsume(e: Exp, expected: Type, gamma: Map[String, Type]): Unit = {
  val actual = infer(e, gamma)
  if (actual != expected)
    sys.error("type mismatch: expected " + expected + " but inferred " + actual)
}

def typeCheck(e: Exp, gamma: Map[String, Type]): Type = infer(e, gamma)

val idNum = TypeAscription(Fun("x", "x"), FunType(NumType(), NumType()))
typeCheck(idNum, Map.empty)

val applyTo7 = TypeAscription(
  Fun("f", Ap("f", 7)),
  FunType(FunType(NumType(), NumType()), NumType()))

typeCheck(Ap(applyTo7, Fun("x", Add("x", 1))), Map.empty)
eval(Ap(applyTo7, Fun("x", Add("x", 1))))

val leftVal = TypeAscription(SumLeft(3), SumType(NumType(), JunitType()))

val caseExpr =
  TypeAscription(
    EliminateSum(leftVal, Fun("n", Add("n", 1)), Fun("u", 0)),
    NumType())

typeCheck(caseExpr, Map.empty)
eval(caseExpr)

typeCheck(Fun("x", "x"), Map.empty)

