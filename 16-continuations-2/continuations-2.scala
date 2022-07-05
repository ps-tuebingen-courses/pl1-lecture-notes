import scala.language.implicitConversions

enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp)
  case Ap(funExpr: Exp, argExpr: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)

import Exp._

sealed abstract class CPSExp
abstract class CPSVal extends CPSExp
case class CPSNum(n: Int) extends CPSVal
case class CPSAdd(l: CPSVar, r: CPSVar) extends CPSVal
case class CPSCont(v: String, body: CPSExp) extends CPSVal
case class CPSFun(x: String, k: String, body: CPSExp) extends CPSVal
case class CPSVar(x: String) extends CPSVal { override def toString = x.toString }
implicit def id2cpsexp(x: String): CPSVar = CPSVar(x)

case class CPSContAp(k: CPSVal, a: CPSVal) extends CPSExp
// the arguments are even CPSVar and not only CPSVal!
case class CPSFunAp(f: CPSVar, a: CPSVar, k: CPSVar) extends CPSExp

def freeVars(e: Exp): Set[String] =  e match {
   case Id(x) => Set(x)
   case Add(l, r) => freeVars(l) ++ freeVars(r)
   case Fun(x, body) => freeVars(body) - x
   case Ap(f, a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
}

def freshName(names: Set[String], default: String): String = {
  var last: Int = 0
  var freshName = default
  while (names contains freshName) { freshName = default + last; last += 1; }
  freshName
}

def cps(e: Exp): CPSCont = e match {
   case Add(e1, e2) => {
     val k = freshName(freeVars(e), "k")
     val lv = freshName(freeVars(e2), "lv")
     CPSCont(k, CPSContAp(cps(e1), CPSCont(lv, CPSContAp(cps(e2), CPSCont("rv", CPSContAp(k, CPSAdd("rv", lv)))))))
   }
   case Fun(a, body) => {
     val k = freshName(freeVars(e), "k")
     val dynk = freshName(freeVars(e), "dynk")
     CPSCont(k, CPSContAp(k, CPSFun(a, dynk, CPSContAp(cps(body), dynk))))
   }
   case Ap(f, a) => {
     val k = freshName(freeVars(e), "k")
     val fval = freshName(freeVars(a), "fval")
     CPSCont(k, CPSContAp(cps(f), CPSCont(fval, CPSContAp(cps(a), CPSCont("aval", CPSFunAp(fval, "aval", k))))))
   }
   case Id(x) => {
     val k = freshName(freeVars(e), "k")
     CPSCont(k, CPSContAp(k, CPSVar(x)))
   }
   case Num(n) => {
     val k = freshName(freeVars(e), "k")
     CPSCont(k, CPSContAp("k", CPSNum(n)))
   }
}

