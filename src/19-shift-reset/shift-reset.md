# Shift Reset

The content of this chapter is available as a scala file [here.](./shift-reset.scala)


```scala mdoc
/**
Delimited Continuations
=======================
The continuations we have seen so far represent the whole call-stack. Invoking a continuation
was not like a function call because continuations never return a result. Invoking
a continuation is more similar to a disciplined version of GOTO.
However, the fact that continuations never return and represent the full call-stack often
makes their use cumbersome. In particular, such continuations cannot be composed. The
non-composability of continuations is visible in the fact that applications using 
first-class continuations often need to make use of mutable state.
This is different with _delimited continuations_. Delimited continuations only represent
a part of the call-stack, namely the part up to the next invocation of ``reset``. Delimited continuations
behave like functions, that is, they return a value and are hence composable.
Delimited continuations have many quite powerful applications, ranging from advanced exception handling
to backtracking algorithms and probabilistc programming as well as so-called algebraic effects.
Delimited continuations are available in Racket and many variants of Scheme, OCaML, Haskell and,
thanks to work in our research group, in Java.
Fortunately, a definitional interpreter for delimited continuations is pretty simple.
*/

sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Fun(param: Symbol, body: Exp) extends Exp
implicit def num2exp(n: Int): Exp = Num(n)
implicit def id2exp(s: Symbol): Exp = Id(s)
case class App (funExpr: Exp, argExpr: Exp) extends Exp
case class Shift(param: Symbol, body: Exp) extends Exp
case class Reset(body: Exp) extends Exp

sealed abstract class Value
type Env = Map[Symbol, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value
case class ContV(f: Value => Value) extends Value

def eval(e: Exp, env: Env, k: Value => Value) : Value = e match {
  case Num(n: Int) => k(NumV(n))
  case Id(x) => k(env(x))
  case Add(l,r) => {
    eval(l,env, lv => 
        eval(r,env, rv =>
          (lv,rv) match {
            case (NumV(v1), NumV(v2)) => k(NumV(v1+v2))
            case _ => sys.error("can only add numbers")
          }))
  }
  case f@Fun(param,body) => k(ClosureV(f, env))
  
  case App(f,a) => eval(f,env, cl => cl match {
            case ClosureV(f,closureEnv) => eval(a,env, av => eval(f.body, closureEnv + (f.param -> av),k))
            case ContV(k2) => eval(a,env, av => k(k2(av))) // compose continuations k2 and k
            case _ => sys.error("can only apply functions")
  })
  case Reset(e) => k(eval(e,env,x=>x)) // reset the continuation to the identity function
  case Shift(param,body) => eval(body, env+(param -> ContV(k)), x=>x)  // wrap current continuation and reset continuation
}


/** 
References:
Olivier Danvy and Andre Filinski, “Abstracting Control,” LISP and Functional Programming, 1990.
O. Kiselyov, An argument against call/cc. http://okmij.org/ftp/continuations/against-callcc.html
O. Kiselyov, Introduction to programming with shift and reset. http://okmij.org/ftp/continuations/#tutorial
*/

```
