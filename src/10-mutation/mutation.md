# Mutation

The content of this chapter is available as a scala file [here.](./mutation.scala)

```scala mdoc
import scala.language.implicitConversions

/**
Mutation
========
Today we study _mutation_. More specifically, we want to equip our language with mutable data structures.
Typical mutable data structures in common languages include objects with mutable fields or structures/records in languages like C or Pascal.
We will study a particularly simple mutable data structure: Boxes. In OO parlance, boxes can be thought of as an object with
a single field that can be mutated. Despite their simplicity, boxes already illustrate all main issues associated with adding
mutable state to a language.
A different and less interesting form of mutation is the mutability of _variables_, such as the possibility to assign something
to a 'local' variable bound via a lambda or ``with``. We will not talk about mutable variables today.
We will add boxes to our base language, FAE.
*/
object Syntax {
  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Id(name: String) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Mul(lhs: Exp, rhs: Exp) extends Exp
  case class If0(cond: Exp, thenExp: Exp, elseExp: Exp) extends Exp
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)
  case class Fun(param: String, body: Exp) extends Exp
  case class Ap (funExpr: Exp, argExpr: Exp) extends Exp
  def wth(x: String, xdef: Exp, body: Exp) : Exp = Ap(Fun(x,body),xdef)

  /**
  To add mutation to FAE, we add four language constructs:
  */

  case class NewBox(e: Exp) extends Exp // create a new box
  case class SetBox(b: Exp, e: Exp) extends Exp // assign to a box
  case class OpenBox(b: Exp) extends Exp // read value in a box
  case class Seq(e1: Exp, e2: Exp) extends Exp // sequencing of expressions
}
import Syntax._
/**
In this new language, the following sample program,
*/

val test1 = wth("b", NewBox(0),
              Seq(
                SetBox("b", Add(1, OpenBox("b"))),
                OpenBox("b")))

/**
should give as result ``1`` in a proper implementation.
Let's consider how our interpreter could handle sequencing.
Here is an attempt:
     case Seq(e1, e2) => {
       eval(e1, env)
       eval(e2, env)
     }
This cannot be correct. As long as our interpreter does not use mutation, evaluation could not make any changes to the environment,
hence there is* no way the evaluation of e1 could have any effect on the evaluation of e2.
In order to demostrate the actual nature of mutation, we will not use mutation in our meta-language to implement mutation
in our object language. That said, we will not use a mutable data structure to implement environment in our interpreter.
Instead, one may turn to the so-called environment-passing style, in which the interpreter returns also a possibly updated environment
together with the computed value when it evaluates an expression.  However, this solution does not always work.  Consider the following
example:
*/

val test2 = wth("a", NewBox(1),
              wth("f", Fun("x", Add("x", OpenBox("a"))),
                Seq(SetBox("a",2),
                  Ap("f", 5))))

/**
The mutation should affect the box stored in the closure bound to ``f``.  But with the implementation strategy described above it would not.
Note that changing the value of a in the example is not a vialation of static scope.  Scoping only says where an identifier is bound;
it does not say to what an identifier is bound, in particular, whether whatever bound to the identifier is fixed.
Indeed, the variable a is bound to the same box in both the static environment where the function f is created and the dynamic environment
where the function f is applied.
As before, when applying the function f to the argument 5, we can choose either
   1) To use the static environment (where the variable a is bound to a boxed 1) stored in the closure created for f.
   2) Or to use the dynamic environment (where the variable a is bound to a  boxed 2) present at the time of applying f.
The first choice leads the program to evaluate to 6 rather than the expected 7.  The second will record the change to the box,
but it reintroduces dynamic scoping.  So both choices do not work.
Insight: We need _two_ repositories of information.
One, the environment, guards static scope.
*/

sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

/**
The other, which we call _store_, is trackis dynamic changes.
Determining the value inside a box will become a two-step process: We first evaluate the box expression to an _address_,
and then use the store to lookup the value stored at that address. We choose to represent addresses by integers.
*/

type Address = Int
case class AddressV(a: Address) extends Value

type Store = Map[Address, Value]

/**
We will often need a fresh address in the store. We do so using a counter variable.
*/

var _nextAddress = 0

def nextAddress : Address = {
  _nextAddress += 1
  _nextAddress
}

/**
Note: We promised to implement the interpreter without using mutation. Here we did use mutation, but this musage of mutation
is not essential: we could instead just search for the largest address in the present store and add one to it.
Let's now discuss the evaluation of FAE with conditionals and boxes, BCFAE. To this end, consider the following sample program:
*/

val test3 = wth("switch", NewBox(0),
             wth("toggle", Fun("dummy", If0(OpenBox("switch"),
                                          Seq(SetBox("switch", 1), 1),
                                          Seq(SetBox("switch", 0), 0))),
                 Add(Ap("toggle",42), Ap("toggle",42))))

/**
This program should return 1. Let's discuss on the blackboard what the environment and store should look like during the
evaluation of this program.
ID      Exp                     Value   Env             Store
A       wth(..                          "switch" -> ..   1 -> NumV(0)
B        wth(..                         "toggle" -> ..
C         Add(..
D          Ap("toggle")         1                       1 -> NumV(1)
E          Ap("toggle")         0                       1 -> NumV(0)
F         Add(0,1)              1
Insight:
We must pass the current store in to evaluate every expression and pass the possibly updated store out after the evaluation.
This is called _store-passing style:.  Consequently, we have to update the type of our evaluator.
*/

def eval(e: Exp, env: Env, s: Store) : (Value, Store) = e match {
  /* All expressions whose evaluation does not alter the store just return s. */
  case Num(n) => (NumV(n), s)
  case Id(x) => (env(x), s)
  case f@Fun(_, _) => (ClosureV(f, env), s)
  /* In recursive cases we have to thread the store through the
   * evaluation. In particular, we define the order of evaluation
   * explicitly through data flow dependencies.  */
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

  /* In a sequence, we ignore the result of evaluating e1 but not its
   * effect on the store. */
  case Seq(e1, e2) => eval(e2, env, eval(e1, env, s)._2)

  /* A new box is created by putting it into the store at a new
   * address.  */
  case NewBox(e: Exp)
    => eval(e, env, s) match {
         case (v, s1) => {
           val a = nextAddress
           (AddressV(a), s1 + (a -> v))
         }
       }

  /* Setting a box is now a two-step process: First evaluate b to an
   * address, then lookup and update the value associated to the
   * address in the store. Note that "updated" is a functional method.  */
  case SetBox(b: Exp, e: Exp)
    => eval(b, env, s) match {
         case (AddressV(a), s1)
           => eval(e, env, s1) match {
                case (ev, s2) => (ev, s2.updated(a, ev))
              }
         case _ => sys.error("can only set boxes")
       }

  /* OpenBox uses the same two-step process but does not update the
   * store.  */
  case OpenBox(b: Exp)
    => eval(b, env, s) match {
         case (AddressV(a), s1) => (s1(a), s1)
         case _                 => sys.error("can only open boxes")
       }
}

/**
From an implementation point of view, our interpreter has the problem that nothing is ever removed from the store.
One possibility would be to add an operation "removeBox" or the like to the language, but this would lead to dangling pointers
and all the* problems associated with manual memory management.
Our model of stores is sufficient to illustrate how modern languages deal with memory management: by garbage collection.
Garbage collectors automatically reclaim memory that is no longer referenced from within the active part of the computation.
We can* model a (naive) mark-and-sweep garbage collector as follows:
*/

def gc(env: Env, store:Store) : Store = {

  def allAddrInVal(v: Value) : Set[Address] = v match {
    case AddressV(a)      => Set(a)
    case NumV(_)          => Set.empty
    case ClosureV(f, env) => allAddrInEnv(env)
  }

  def allAddrInEnv(env: Env) : Set[Address] =
    env.values.map(allAddrInVal _).fold(Set.empty)(_ union _)

  def mark(seed: Set[Address]) : Set[Address] = {
    val newAddresses = seed.flatMap(ad => allAddrInVal(store(ad)))
    if (newAddresses.subsetOf(seed)) seed
    else mark(seed union newAddresses)
  }

  val marked = mark(allAddrInEnv(env)) // mark ...
  store.view.filterKeys(marked(_)).toMap           // and sweep!
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

/**
Note that garbage collectors only _approximate_ the set of semantically disposable store entities. Even with garbage collectors,
applications may very well suffer from memory leaks. The approximation should be _safe_, in the sense that a datum is never reclaimed
when it is used by subsequent computations. Furthermore, it must reclaim enough garbage to be actually useful. Reachability has turned
out to be a rather useful (and sound) approximation of semantic disposability. Garbage collectors must also be efficient.
Efficiency of GC is a huge research topic that we are not going to discuss. One efficiency problem with garbage collectors based on
reachability that we want to mention is the "stop-the-world" phenomenon.
 */
```
