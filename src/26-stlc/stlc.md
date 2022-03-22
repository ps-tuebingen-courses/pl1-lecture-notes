# Simply Typed Lambda Calculus

The content of this chapter is available as a scala file [here.](./stlc.scala)

```scala
import scala.language.implicitConversions

/**
The Simply-Typed Lambda Calculus
================================
We start with the untyped substitution-based lambda calculus augmented by the possibility to add type annotations to function definitions.
The type annotation is ignored by the interpreter.
Why are we using the substitution-based interpreter? Because it is simpler to state the type soundness
theorem. If we had values that are separate from expressions, we would need to define a type system for
these values. This is particularly tricky for closures with their embedded environments.
We also show a couple of standard extensions to the simply-typed lambda calculus:
1. A unit type JunitType() with associated term JUnit()
2. Let-bindings (which don't need type annotations)
3. Type Ascription (corresponding to "::" in Scala, like "5 + 3 :: Int")
4. Products (or Pairs, more specifically)
5. Sums (binary sums, more specifically)
To avoid the problem of "guessing" the "from" type in a function definition, we annotate
function definitions with the expected argument type.
*/

sealed abstract class Type

sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: String) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: String) = Id(s)
case class Fun(param: String, t: Type, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp
case class Junit() extends Exp
case class Let(x: String, xdef: Exp, body: Exp) extends Exp
case class TypeAscription(e: Exp, t: Type) extends Exp

case class Product(e1: Exp, e2: Exp) extends Exp
case class Fst(e: Exp) extends Exp
case class Snd(e: Exp) extends Exp

case class SumLeft(left: Exp, right: Type) extends Exp
case class SumRight(left: Type, right: Exp) extends Exp
case class EliminateSum(e: Exp, fl: Exp, fr: Exp) extends Exp


def freshName(names: Set[String], default: String) : String = {
  var last : Int = 0
  var freshName = default
  while (names contains freshName) { freshName = default+last.toString; last += 1; }
  freshName
}

def freeVars(e: Exp) : Set[String] =  e match {
   case Id(x) => Set(x)
   case Add(l,r) => freeVars(l) ++ freeVars(r)
   case Fun(x,_,body) => freeVars(body) - x
   case App(f,a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
   case Junit() => Set.empty
   case TypeAscription(e,t) => freeVars(e)
   case Let(x,xdef,body) => freeVars(xdef) ++ (freeVars(body) - x)
   case Product(e1,e2) => freeVars(e1) ++ freeVars(e2)
   case Fst(e) => freeVars(e)
   case Snd(e) => freeVars(e)
   case SumLeft(e,_) => freeVars(e)
   case SumRight(_,e) => freeVars(e)
   case EliminateSum(e,fl,fr) => freeVars(e) ++ freeVars(fl) ++ freeVars(fr)
}

def subst(e1 : Exp, x: String, e2: Exp) : Exp = e1 match {
  case Num(n) => e1
  case Junit() => e1
  case Add(l,r) => Add(subst(l,x,e2), subst(r,x,e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case App(f,a) => App(subst(f,x,e2),subst(a,x,e2))
  case TypeAscription(e,t) => TypeAscription(subst(e,x,e2),t)
  case Fun(param,t,body) =>
    if (param == x) e1 else {
      val fvs = freeVars(body) ++ freeVars(e2)
      val newvar = freshName(fvs, param)
      Fun(newvar, t,subst(subst(body, param, Id(newvar)), x, e2))
    }
  case Let(y,ydef,body) =>
    if (x == y) Let(y,subst(ydef,x,e2),body) else {
      val fvs = freeVars(body) ++ freeVars(e2)
      val newvar = freshName(fvs,y)
      Let(newvar,subst(ydef,x,e2),subst(subst(body,y,Id(newvar)),x,e2))
    }
  case Product(a,b) => Product(subst(a,x,e2),subst(b,x,e2))
  case Fst(e) => Fst(subst(e,x,e2))
  case Snd(e) => Snd(subst(e,x,e2))
  case SumLeft(e,t) => SumLeft(subst(e,x,e2),t)
  case SumRight(t,e) => SumRight(t,subst(e,x,e2))
  case EliminateSum(e,fl,fr) => EliminateSum(subst(e,x,e2),subst(fl,x,e2),subst(fr,x,e2))
}

def eval(e: Exp) : Exp = e match {
  case Id(v) => sys.error("unbound identifier: " + v.name)
  case Add(l,r) => (eval(l), eval(r)) match {
                     case (Num(x),Num(y)) => Num(x+y)
                     case _ => sys.error("can only add numbers")
                    }
  case App(f,a) => eval(f) match {
     case Fun(x,_,body) => eval( subst(body,x, eval(a)))  // call-by-value
     case _ => sys.error("can only apply functions")
  }
  case TypeAscription(e,_) => eval(e)
  case Let(x,xdef,body) => eval(subst(body,x,eval(xdef)))
  case Product(a,b) => Product(eval(a),eval(b))
  case Fst(e) => eval(e) match {
    case Product(a,b) => a
    case _ => sys.error("can only apply Fst to products")
  }
  case Snd(e) => eval(e) match {
    case Product(a,b) => b
    case _ => sys.error("can only apply Snd to products")
  }
  case SumLeft(e,t) => SumLeft(eval(e),t)
  case SumRight(t,e) => SumRight(t,eval(e))
  case EliminateSum(e,fl,fr) => eval(e) match {
    case SumLeft(e2,_) => eval(App(fl,e2))
    case SumRight(_,e2) => eval(App(fr,e2))
    case _ => sys.error("can only eliminate sums")
  }
  case _ => e // numbers and functions evaluate to themselves
}

/**
We classify values into three types: Booleans, integers, and function types. For function types, we need some abstraction for its input
and output; otherwise the type checker cannot be compositional. Luckily we do already have such an abstraction, namely types.
Hence ``Funtype`` becomes a recursive data type.
*/

case class NumType() extends Type
case class FunType(from: Type, to: Type) extends Type
case class JunitType() extends Type
case class ProductType(fst: Type, snd: Type) extends Type

case class SumType(left: Type, right: Type) extends Type

/**
The type checker for the so-called _Simply-Typed Lambda Calculus_  (STLC). To deal with identifiers, we need an abstraction of environments.
A type environment has the form ``Map[String,Type]``.
The type checker for the STLC is as follows:
*/


def typeCheck(e: Exp, gamma: Map[String,Type]) : Type = e match {
  case Num(n) => NumType()
  case Junit() => JunitType()
  case Id(x) => gamma.get(x) match {
    case Some(t) => t
    case _ => sys.error("free variable: " ++ x.toString)
  }
  case Add(l,r) => (typeCheck(l,gamma),typeCheck(r,gamma)) match {
    case (NumType(),NumType()) => NumType()
    case _ => sys.error("Type error in Add")
  }
  case Fun(x,t,body) => FunType(t, typeCheck(body, gamma + (x -> t)))
  case App(f,a) => {
    typeCheck(f,gamma) match {
      case FunType(from,to) => if (from == typeCheck(a,gamma)) to else sys.error("type error: arg does not match expected type")
      case _ => sys.error("first operand of app must be a function")
    }
  }
  case Let(x,xdef,body) => typeCheck(body, gamma + (x -> typeCheck(xdef,gamma)))
  case TypeAscription(e,t) => if (typeCheck(e,gamma) == t) t else sys.error("type error in ascription")
  case Product(e1,e2) => ProductType(typeCheck(e1,gamma),typeCheck(e2,gamma))
  case Fst(e) => typeCheck(e,gamma) match {
    case ProductType(t1,t2) => t1
    case _ => sys.error("can only project Products")
  }
  case Snd(e) => typeCheck(e,gamma) match {
    case ProductType(t1,t2) => t2
    case _ => sys.error("can only project Products")
  }
  case SumLeft(e,t) => SumType(typeCheck(e,gamma), t)
  case SumRight(t,e) => SumType(t, typeCheck(e,gamma))
  case EliminateSum(e,fl,fr) => typeCheck(e,gamma) match {
    case SumType(left,right) => (typeCheck(fl,gamma), typeCheck(fr,gamma)) match {
      case (FunType(leftf,t1),FunType(rightf,t2)) if ((left == leftf) && (right == rightf)) =>
        if (t1 == t2) t1 else sys.error("type error: functions must have same return type")
      case _ => sys.error("type error in EliminateSum: second and third argument must be functions")
    }
    case _ => sys.error("type error: can only eliminate sums")
  }

}

/* Soundness of Simply-Typed Lambda Calculus (STLC) :
If e : Exp und typeCheck(e,Map.empty) == t, then typeCheck(eval(e),Map.empty) == t
*/
```