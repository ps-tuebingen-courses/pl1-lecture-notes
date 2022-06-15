# Lazy Evaluation

The content of this chapter is available as a Scala file [here.](./lazy-evaluation.scala)


```scala mdoc:invisible
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
    def wth(x: String, xdef: Exp, body: Exp) : Exp = Ap(Fun(x,body),xdef)
}

import Syntax._
import Exp._

def freshName(names: Set[String], default: String) : String = {
  var last : Int = 0
  var freshName = default
  while (names contains freshName) { freshName = default+last; last += 1; }
  freshName
}

def freeVars(e: Exp) : Set[String] =  e match {
   case Id(x) => Set(x)
   case Add(l,r) => freeVars(l) ++ freeVars(r)
   case Fun(x,body) => freeVars(body) - x
   case Ap(f,a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
}

def subst(e1 : Exp, x: String, e2: Exp) : Exp = e1 match {
  case Num(n) => e1
  case Add(l,r) => Add(subst(l,x,e2), subst(r,x,e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case Ap(f,a) => Ap(subst(f,x,e2),subst(a,x,e2))
  case Fun(param,body) =>
    if (param == x) e1 else {
      val fvs = freeVars(Fun(param,body)) ++ freeVars(e2) + x
      val newvar = freshName(fvs, param)
      Fun(newvar, subst(subst(body, param, Id(newvar)), x, e2))
    }
}

def eval(e: Exp) : Exp = e match {
  case Id(v) => sys.error("unbound identifier: " + v)
  case Add(l,r) => (eval(l), eval(r)) match {
                     case (Num(x),Num(y)) => Num(x+y)
                     case _ => sys.error("can only add numbers")
                    }
  case Ap(f,a) => eval(f) match {
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
  case Ap(f,a) => eval2(f) match {
     case Right(Fun(x,body)) => eval2( subst(body,x, eval(a)))
     case _ => sys.error("can only apply functions")
  }
  case f@Fun(_,_) => Right(f)
  case n@Num(_) => Left(n)
}

val test = Ap( Fun("x",Add("x",5)), 7)

val omega = Ap(Fun("x",Ap("x","x")), Fun("x",Ap("x","x")))
// try eval(omega) to crash the interpreter ;-)

val test2 = wth("x", 5, Ap(Fun("f", Ap("f",3)), Fun("y",Add("x","y"))))

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
  case Ap(f,a) => evalWithEnv(f,env) match {
    // Use environment stored in closure to realize proper lexical scoping!
    case ClosureV(f,closureEnv) => evalWithEnv(f.body, closureEnv + (f.param -> evalWithEnv(a,env)))
    case _ => sys.error("can only apply functions")
  }
}
```
## Motivation for Lazy Evaluation

Read "Why Functional Programming Matters" by John Hughes available at http://www.cse.chalmers.se/~rjmh/Papers/whyfp.html

What lazy evaluation means:

The choice of evaluation strategy is a purely semantic change that requires no change to the syntax.
For this reason we reuse the syntactic definitions of FAE, hence load
(higher-order-functions.scala)[https://ps-tuebingen-courses.github.io/pl1-lecture-notes/07-higher-order-functions/higher-order-functions.scala]
before executing this script.
Before we discuss lazy evaluation, we will first discuss a related evaluation strategy, _call-by-name_.

_Call-by-name_ can be explained very succintly in the substitution-based interpreter: Instead of evaluating the argument `a` in the
`Ap` case before substitution, we substitute the unevaluated argument into the body. The rest remains exactly the same.

```scala mdoc
def evalcbn(e: Exp) : Exp = e match {
  case Id(v) => sys.error("unbound identifier: " + v)
  case Add(l,r) => (evalcbn(l), evalcbn(r)) match {
                     case (Num(x),Num(y)) => Num(x+y)
                     case _ => sys.error("can only add numbers")
                    }
  case Ap(f,a) => evalcbn(f) match {
     case Fun(x,body) => evalcbn( subst(body,x, a)) // no evaluation of a!
     case _ => sys.error("can only apply functions")
  }
  case _ => e
}
```

Does this change the semantics, or is it just an implementation detail? In other words, is ``eval(e) == evalcbn(e)`` for all ``e`` ?
Let's try two former test cases.

```scala mdoc
assert(evalcbn(test) == eval(test))
assert(evalcbn(test2) == eval(test2))
```

One can formally prove that if `eval` and `evalcbn` both produce a number then the numbers are equal. Do they also agree if they produce
a function?
Not necessarily. Consider:

```scala mdoc:silent
val test3 = Ap(Fun("f",Fun("x",Ap("f","x"))),Add(1,2))

assert(eval(test3) == Fun("x",Ap(Num(3),Id("x"))))
assert(evalcbn(test3) == Fun("x",Ap(Add(Num(1),Num(2)),Id("x"))))
```

However, if both produce a function, then the functions "behave" the same. More specifically, the function bodies produced by `eval`
may be "more evaluated" than those produced by `evalcbn` since for the latter the evaluation of the arguments substituted for the parameters
in the body is deferred. If we evaluated within function bodies (also called evaluation "under a
lambda") - which our interpreters do not do - we could produce the expression returned from `eval` from the expression returned by `evalcbn`.
This kind of equivalence is also called "beta-equivalence".

Most importantly, however, `eval` and `evalcbn` differ with regard to their termination behavior. We have seen that `omega` is a diverging
expression. In `eval`, the term

```scala mdoc:silent
 val test4 = Ap(Fun("x",5),omega)
```

is hence also diverging. In contrast:

```scala mdoc
assert(evalcbn(test4) == Num(5))
```

## Extra material: Infinite lists in FAE (not relevant for exam)

Using our call-by-name interpreter, we can express the same kinds of programming patterns that we have tried in Haskell, such as
infinite lists.

We do not have direct support for lists, but we can encode lists. This kind of encoding is called _Church encoding_.

```scala mdoc:silent
val nil = Fun("c", Fun("e", "e"))
val cons  = Fun("x", Fun("xs", Fun("c", Fun("e", Ap(Ap("c", "x"), Ap(Ap("xs", "c"),"e"))))))
```

For instance, the list 1,2,3 is encoded as:

```scala mdoc:silent
val list123 = Ap(Ap("cons",1),Ap(Ap("cons",2),Ap(Ap("cons",3), "nil")))
```

The map function on lists becomes :
```scala mdoc:silent
val maplist = Fun("f", Fun("l", Ap(Ap("l", Fun("x", Fun("xs", Ap(Ap("cons", Ap("f","x")),"xs")))), "nil")))
```

For instance, we can map the successor function over the 1,2,3 list.

```scala mdoc:silent
val test5 = wth("cons",cons,
            wth("nil", nil,
            wth("maplist", maplist,
            Ap(Ap("maplist", Fun("x", Add("x",1))), list123))))
```

Since it is somewhat difficult to print out the resulting list in our primitive language we construct the result we expect explicitly.

```scala mdoc:silent
val test5res = wth("cons",cons,
               wth("nil", nil,
                 Ap(Ap("cons",2),Ap(Ap("cons",3),Ap(Ap("cons",4), "nil")))))
assert(eval(test5) == eval(test5res))
```

Using `evalcbn` instead of `eval` the assertion does not hold (why?), but the results are beta-equivalent.
We can also construct infinite lists. To this end, we need some form of recursion. We choose the standard fixed-point operator Y.
This operator only works under call-by-name or other so-called "non-strict" evaluation strategies.

```scala mdoc:silent
val y = Fun("f", Ap(Fun("x",Ap("f", Ap("x","x"))), Fun("x",Ap("f",Ap("x","x")))))
```

Using Y, we can construct infinite lists, such as the list of all natural numbers.

```scala mdoc:silent
val allnats = Ap(Ap("y", Fun("nats", Fun("n", Ap(Ap("cons","n"), Ap("nats", Add("n",1)))))),1)
```

We can also perform standard computations on infinite lists, such as mapping the successor function over it.

```scala mdoc:silent
val list2toinfty = wth("cons",cons,
                   wth("nil", nil,
                   wth("y", y,
                   wth("maplist", maplist,
                      Ap(Ap("maplist", Fun("x", Add("x",1))), allnats)))))
```

Of course, ``list2toinfty`` diverges when we use ``eval``, but it works fine with ``evalcbn``. It is hard to verify the result due to
an almost unreadable output. Hence we propose the following

Exercise: Extend the language such that you can implement the "take" function as known from Haskell within the language
(if0-expressions or something like it are needed for that). Now add a "print" function that prints a number on the console.
Use it to display that the first 3 list elements of `list2toinfty` are 2,3,4.

_end of extra material_

## Environment-based lazy evaluation

Let us now consider the question how we can implement call-by-name in the environment-based interpreter. Translating the idea of not
evaluating the function argument to the environment-based version seems to  suggest that the environment should map identifiers to
expression instead of values.

However, we run into the same problems that we had with first-class functions before we introduced closures: What happens to the
deferred substitutions that still have to be applied in the function argument? If we discard the environment in which the function
argument was defined we again introduce a variant of dynamic scoping.

Hence, like for closures, we need to store the environment together with the expression. We call such a  pair a _thunk_. An environment
hence becomes a mapping from symbols to thunks. Note that environments and thunks are hence mutually recursive. In Scala, we can hence
not use type definitions of the form

```
   type Thunk = (Exp, Env)
   type Env = Map[String, Thunk]
```

Instead, we use a Scala class Env to express this recursion.
Since we want to experiment with different variants of how to generate and evaluate thunks we first create a parameterizable variant
of the evaluator that leaves open how to
  1. represent thunks (type `Thunk`)
  2. create thunks (method `delay`)
  3. evaluate thunks (method `force`).
__Hint__: Research on the internet what abstract type members in Scala are. For instance, here: http://www.scala-lang.org/node/105

```scala mdoc
trait CBN {
    type Thunk

    case class EnvThunk(map: Map[String, Thunk]) {
      def apply(key: String) = map.apply(key)
      def +(other: (String, Thunk)) : EnvThunk = EnvThunk(map+other)
    }

    def delay(e: Exp, env: EnvThunk) : Thunk
    def force(t: Thunk) : ValueCBN

    // since values also depend on EnvThunk and hence on Thunk they need to
    // be defined within this trait
    sealed abstract class ValueCBN
    case class NumV(n: Int) extends ValueCBN
    case class ClosureV(f: Fun, env: EnvThunk) extends ValueCBN
    def evalCBN(e: Exp, env: EnvThunk) : ValueCBN = e match {
      case Id(x) => force(env(x)) // force evaluation of thunk if identifier is evaluated
      case Add(l,r) => {
        (evalCBN(l,env), evalCBN(r,env)) match {
          case (NumV(v1),NumV(v2)) => NumV(v1+v2)
          case _ => sys.error("can only add numbers")
        }
      }
      case Ap(f,a) => evalCBN(f,env) match {
        // delay argument expression and add it to environment of the closure
        case ClosureV(f,cenv) => evalCBN(f.body, cenv + (f.param -> delay(a,env)))
        case _ => sys.error("can only apply functions")
      }
      case Num(n) => NumV(n)
      case f@Fun(x,body) => ClosureV(f,env)
    }
}
```

Let's now create an instance of CBN that corresponds to the substitution-based call-by-name interpreter. A thunk is just a pair
of expression and environment. Forcing a thunk just evaluates it in the stored environment.
To understand what is going on during evaluation of tests we trace argument evaluation by a  printout to the console.

```scala mdoc
object CallByName extends CBN {
  type Thunk = (Exp,EnvThunk)
  def delay(e: Exp, env: EnvThunk) = (e,env)
  def force(t: Thunk) = {
    println("Forcing evaluation of expression: "+t._1)
    evalCBN(t._1,t._2)
  }
}

assert(CallByName.evalCBN(test, CallByName.EnvThunk(Map.empty)) == CallByName.NumV(12))
```

## Call-by-need

Call-by-name is rather wasteful: If an argument is used _n_ times in the body, the argument expression is re-evaluated _n_-times.
For instance, in

```scala mdoc:silent
val cbntest = wth("double", Fun("x", Add("x","x")),
               Ap("double", Add(2,3)))
```

the sum of 2 and 3 is computed twice.  If the argument is passed again to another function, this may lead to an exponential blow-up.

Example:
```mdoc:silent
val blowup  = wth("a", Fun("x", Add("x","x")),
              wth("b", Fun("x", Add(Ap("a","x"), Ap("a","x"))),
              wth("c", Fun("x", Add(Ap("b","x"), Ap("b","x"))),
              wth("d", Fun("x", Add(Ap("c","x"), Ap("c","x"))),
              wth("e", Fun("x", Add(Ap("d","x"), Ap("d","x"))),
              wth("f", Fun("x", Add(Ap("e","x"), Ap("e","x"))),
              Ap("f", Add(2,3))))))))
```
Can we do better? Yes, by caching the value when the argument expression is evaluated for the first time. This evaluation strategy is
called _call-by-need_.

Caching is easy to implement in Scala:

```scala mdoc
object CallByNeed extends CBN {
  case class MemoThunk(e: Exp, env: EnvThunk) {
    var cache: ValueCBN = null
  }
  type Thunk = MemoThunk
  def delay(e: Exp, env: EnvThunk) = MemoThunk(e,env)
  def force(t: Thunk) = {
    if (t.cache == null) {
      println("Forcing evaluation of expression: "+t.e)
      t.cache = evalCBN(t.e, t.env)
      t.cache
    } else println ("Reusing cached value "+t.cache+" for expression "+t.e)
    t.cache
  }
}
```

For instance, compare call-by-need and call-by-name in `cbntest` or `blowup`.
However, the meta-language (i.e., the subset of Scala features) used in the interpreter has become more complicated:

Since we are using mutation, the order of evaluation and aliasing of object references becomes important.
Luckily, call-by-need agrees with call-by-name with regard to produced values and termination behavior, hence it is usually not
necessary to reason about programs with the call-by-need semantics. If, however, one wants to reason about the performance of a
program in a call-by-need setting, one has to take these additional complications into account.
In practice, it is even worse, since languages like Haskell perform additional optimizations that, for instance, switch to call-by-value if an analysis can
determine that an argument will definitely be used (lookup "strictness analysis").

Topics for class discussion:
 - Is it a good idea to mix a language with implicit mutation (such as Java, Scala, C++, Python, ...) with lazy evaluation?
 - How can one simulate lazy evaluation in an eager language? Basic idea: 'Lambda' as evaluation firewall.
