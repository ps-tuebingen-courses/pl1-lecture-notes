# Defunctionalization, Refunctionalization

The content of this chapter is available as a Scala file [here.](./defunctionalization.scala)

In the discussion of syntactic interpretation vs. meta interpretation we have learned that we only learn something about (and control)
a language feature if we choose syntactic interpretation.

Today we want to discuss techniques with which we can make our interpreter so syntactic that it corresponds to an abstract machine:
A machine with a (possibly infinite) set of states and a simple transition relation between the states. We already know the technique
with which we can take control over the call stack management: _CPS transformation_. After CPS-transforming the interpreter, we do not
rely on the order of evaluation and call stack management of the meta-language anymore. We replicate its definition here:

```scala mdoc
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

sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

object CPSTransformed {
  def eval[T](e: Exp, env: Env, k: Value => T): T = e match {
    case Num(n: Int) => k(NumV(n))
    case Id(x) => k(env(x))
    case Add(l, r) =>
      eval(l, env,
           lv => eval(r, env,
                      rv => (lv, rv) match {
                          case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
                          case _ => sys.error("can only add numbers")
                        }))
    case f@Fun(param, body) => k(ClosureV(f, env))
    case Ap(f, a) =>
      eval(f, env,
           cl => cl match {
              case ClosureV(f, closureEnv) =>
                eval(a, env,
                     av => eval(f.body, closureEnv + (f.param -> av), k))
              case _ => sys.error("can only apply functions")
            })
  }
}
```

However, the CPS-transformed interpreter still uses high-level features of the meta-language, most notably first-class functions.
We will now introduce one transformation that can be used to transform a program using higher-order functions into one using only
first-order functions. It is a general program transformation technique, not restricted only to interpreters.

## Lambda Lifting

The first of these techniques is _lambda lifting_. The goal of lambda lifting is to turn local functions into top-level functions.
That is, all "lambdas" only occur at the top-level. Variables in the local environment that are normally stored in the function's
closure are instead passed as parameters to the top-level function. Lambda lifting is accomplished by the following steps:

 1. Invent a new and unique name for each function that is not a top-level function.
 2. Create a function with this name. Its body is the body of the former local function. Such a function will contain free variables.
 3. Add a parameter to the so-obtained top-level function for each free variable in its body.
    Hence, it becomes a higher-order function that returns a function when passed these arguments.
 4. Replace the local function by a call to the new top-level function and pass the corresponding local context via the arguments
    created in step 3.

__Example__: Let's lambda-lift the functions ``y => y + n`` and ``y => y * n`` in

```scala mdoc
def map(f: Int => Int, xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case (x :: xs) => f(x) :: map(f, xs)
}

def addAndMultNToList(n: Int, xs: List[Int]) = map(y => y * n, map(y => y + n, xs))
```

We create two new top-level functions, let's call them ``f`` and ``g``. Their bodies are respectively ``y => y + n`` and ``y => y * n``.
We add a parameter for each free variable. In the example, the free variable is ``n`` in both cases:

```scala mdoc
def fLam(n: Int) = (y: Int) => y + n
def gLam(n: Int) = (y: Int) => y * n
```

or shorter:

```scala mdoc
def f(n: Int)(y: Int) = y + n
def g(n: Int)(y: Int) = y * n
```

The local function can now be replaced by a call to the new global function.

```scala mdoc
def addAndMultNToListLifted(n: Int, xs: List[Int]) = map(g(n)(_), map(f(n)(_), xs))
```

Let's now perform the same technique to the CPS-transformed interpreter given above. It contains local functions in four places:
two in the ``Add`` branch and two in the ``Ap`` branch. We call the corresponding top-level functions, from left to right,
``addC1``, ``addC2``, ``apC1`` and ``apC2``.

An interesting novelty in the interpreter is that some local functions (corresponding to ``addC1`` and ``apC1``) create local
functions themselves. This means that ``addC1`` must call ``addC2`` and ``apC1`` must call ``apC2``. The rest of the transformation
is a straightforward application of the transformation steps described above:

```scala mdoc
object LambdaLifted {
  def addC1[T](r: Exp, env: Env, k: Value => T)(lv: Value) =
    eval(r, env, addC2(lv, k))

  def addC2[T](lv: Value, k: Value => T)(rv: Value) = (lv, rv) match {
    case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
    case _ => sys.error("can only add numbers")
  }

  def apC1[T](a: Exp, env: Env, k: Value => T)(cl: Value) = cl match {
    case ClosureV(f, closureEnv) => eval(a, env, apC2(f, closureEnv, k))
    case _ => sys.error("can only apply functions")
  }

  def apC2[T](f: Fun, closureEnv: Env, k: Value => T)(av: Value) =
    eval(f.body, closureEnv + (f.param -> av), k)

  def eval[T](e: Exp, env: Env, k: Value => T): T = e match {
    case Num(n: Int) => k(NumV(n))
    case Id(x) => k(env(x))
    case Add(l, r) => eval(l, env, addC1(r, env, k))
    case f@Fun(param, body) => k(ClosureV(f, env))
    case Ap(f, a) => eval(f, env, apC1(a, env, k))
  }
}
```

The lambda-lifted interpreter contains no local functions anymore, but it still contains higher-order functions, since `addC1` etc.
return functions that are passed as parameters to other functions.

## Defunctionalization

_Defunctionalization_ is a program transformation technique that turns higher-order programs that have already been lambda-lifted
into first-order programs that contain no higher-order functions anymore.  Any program contains only finitely many function definitions.
The idea of defunctionalization is to assign a unique identifier to each of these function definitions. The function-"dispatch" then
happens in a function ``apply``, which receives the identifier corresponding to a function definition and dispatches the identifier
to the right function body. Every function application within the program is then replaced by a call to the ``apply`` function with
the function identifier as the first argument.

In addition to the unique identifier, the ``apply`` function also needs bindings for the free variables in the function body.
Hence we need to store the values for these free variables along with the unique identifier. Finally, the ``apply`` function needs
to know about the arguments to the function. These become additional parameters of the ``apply`` function.
Let's illustrate defunctionalization in the ``addAndMultNToList`` example from above.

```scala mdoc
enum FunctionValue:
  case F(n: Int)
  case G(n: Int)

import FunctionValue._

def apply(f: FunctionValue, y: Int): Int = f match {
  case F(n) => y + n
  case G(n) => y * n
}

def map(f: FunctionValue, xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case (x :: xs) => apply(f, x) :: map(f, xs)
}

def addAndMultNToListDefun(n: Int, xs: List[Int]) = map(G(n), map(F(n), xs))
```

Let's now apply defunctionalization to our CPS-transformed interpreter:

```scala mdoc
object Defunctionalized {

  enum FunctionValue[T]:
    case AddC1(r: Exp, env: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case AddC2(lv: Value, k: FunctionValue[T]) extends FunctionValue[T]
    case ApC1(a: Exp, env: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case ApC2(f: Fun, closureEnv: Env, k: FunctionValue[T]) extends FunctionValue[T]

  import FunctionValue._

  def apply[T](fv: FunctionValue[T], v: Value): T  = fv match {
    case AddC1(r, env, k) => eval(r, env, AddC2(v, k))
    case AddC2(lv, k) => (lv, v) match {
      case (NumV(v1), NumV(v2)) => apply(k, NumV(v1 + v2))
      case _ => sys.error("can only add numbers")
    }
    case ApC1(a, env, k) => v match {
      case ClosureV(f, closureEnv) => eval(a, env, ApC2(f, closureEnv, k))
      case _ => sys.error("can only apply functions")
    }
    case ApC2(f, closureEnv, k) => eval(f.body, closureEnv + (f.param -> v), k)
  }

  def eval[T](e: Exp, env: Env, k: FunctionValue[T]): T = e match {
    case Num(n: Int) => apply(k, NumV(n))
    case Id(x) => apply(k, env(x))
    case Add(l, r) => eval(l, env, AddC1(r, env, k))
    case f@Fun(param, body) => apply(k, ClosureV(f, env))
    case Ap(f, a) => eval(f, env, ApC1(a, env, k))
  }
}
```

This interpreter can be seen as an abstract machine. The state space of the abstract machine is
(``Exp`` \\( \times \\) ``Env`` \\( \times \\) ``FunctionValue``) \\( \cup \\) (``FunctionValue`` \\( \times \\) ``Value``),
where \\( \times \\) stands for cross product and \\( \cup \\) stands for set union.
Every case in the pattern matches in ``apply`` and ``eval`` can be read as a transition in this state space.

## From Interpreter to Abstract Machine

To see that we can read the above functions as transitions of an abstract machine, let's actually
construct that machine.
Its domain of States is the type ``MachineState[T]``.
The final state of the machine is ``Done(v)``, for some value ``v``.
Its transition function is the function named ``transition``.
We can see every intermediate state of the abstract machine.
Its state is fully described by the (first-order) state type. We are no
longer dependent on call-stack management or higher-order functions of
the meta-language (Scala).

```scala mdoc
object AbstractMachine {
  enum FunctionValue[T]:
    case AddC1(r: Exp, env: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case AddC2(lv: Value, k: FunctionValue[T]) extends FunctionValue[T]
    case ApC1(a: Exp, env: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case ApC2(f: Fun, closureEnv: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case IdentityFV() extends FunctionValue[Value]

  import FunctionValue._

  enum MachineState[T]:
    case EvalState[T](e: Exp, env: Env, fv: FunctionValue[T]) extends MachineState[T]
    case ApplyState[T](fv: FunctionValue[T], v: Value) extends MachineState[T]
    case Done(v: Value) extends MachineState[Value]

  import MachineState._

  def transition[T](s: MachineState[T]): MachineState[T] =
    s match {
      case EvalState(e, env, k) => transitionEval(e, env, k)
      case ApplyState(fv, v) => transitionApply(fv, v)
      case Done(v) => sys.error("already done")
    }

  def transitionEval[T](e: Exp, env: Env, k: FunctionValue[T]): MachineState[T] = e match {
    case Num(n: Int) => ApplyState(k, NumV(n))
    case Id(x) => ApplyState(k, env(x))
    case Add(l, r) =>
      EvalState(l, env, AddC1(r, env, k))
    case f@Fun(param, body) => ApplyState(k, ClosureV(f, env))
    case Ap(f, a) =>  EvalState(f, env, ApC1(a, env, k))
   }

   def transitionApply[T](fv: FunctionValue[T], v: Value): MachineState[T] = fv match {
    case IdentityFV() => Done(v)
    case AddC1(r, env, k) => EvalState(r, env, AddC2(v, k))
    case AddC2(lv, k) => (lv, v) match {
          case (NumV(v1), NumV(v2)) => ApplyState(k, NumV(v1 + v2))
          case _ => sys.error("can only add numbers")
         }
    case ApC1(a, env, k) => v match {
        case ClosureV(f, closureEnv) => EvalState(a, env, ApC2(f, closureEnv, k))
        case _ => sys.error("can only apply functions")
    }
    case ApC2(f, closureEnv, k) => EvalState(f.body, closureEnv + (f.param -> v), k)
  }
}
import AbstractMachine._
import FunctionValue._
import MachineState._
```

Now let's try this out with a concrete example and look at the tract of transitions

```scala mdoc
val test = Ap(Fun("x", Add("x", 1)), 5)
val initMS: MachineState[Value] = EvalState(test, Map.empty, AbstractMachine.FunctionValue.IdentityFV())
val s1 = transition(initMS)
val s2 = transition(s1)
val s3 = transition(s2)
val s4 = transition(s3)
val s5 = transition(s4)
val s6 = transition(s5)
val s7 = transition(s6)
val s8 = transition(s7)
val s9 = transition(s8)
val s10 = transition(s9)
val s11 = transition(s10)
```

We can also automate this into a function that collects the list of all states.

```scala mdoc
def evalMachine(e: Exp): List[MachineState[Value]] = {
  val initMS: MachineState[Value] = EvalState(e, Map.empty, AbstractMachine.FunctionValue.IdentityFV())
  List.unfold(initMS)({ case Done(v) => None
                        case s => { val s2 = transition(s); Some((s, s2))}})
}

val q = evalMachine(test)
```

## Refunctionalization

Can we also invert defunctionalization? That is, can we represent a
program such that a data type is replaced by a function type?

Let's look at an example.

```scala mdoc
enum MyList[T]:
  case MyEmptyList() extends MyList[T]
  case MyCons(x: T, xs: MyList[T]) extends MyList[T]

import MyList._

def nth[T](l: MyList[T], i: Int): T = l match {
  case MyEmptyList() => sys.error("index out of range")
  case MyCons(x, xs) => if (i == 0) then x else nth(xs, i - 1)
}
```

Refunctionalization works smoothly when there is just one pattern match on
a data type. In this case, we represent a list by "its" ``nth`` function:

```scala mdoc
type MyListR[T] = Int => T

def myEmptyList[T]: MyListR[T] = _ => sys.error("index out of range")
def myCons[T](x: T,  xs: MyListR[T]): MyListR[T] =
  i => if (i == 0) then x else xs(i - 1)
```

The defunctionalized and the refunctionalized lists differ in their modular structure.
In the defunctionalized version, it is "easy" (only addition of code but no modification
of existing code necessary) to add new functions that pattern-match on lists, such
as a ``length``function.


```scala mdoc
def length[T](l: MyList[T]): Int = l match {
  case MyEmptyList() => 0
  case MyCons(x, xs) => 1 + length(xs)
}
```

In the refunctionalized version, on the other hand, it is "easy" to add new ways to create lists.
For instance, here is a constructor for the infinite list of numbers

```scala mdoc
def allNats: MyListR[Int] = i => i
```

The two versions hence corresponds to the two dimensions of the aforementioned "expression problem".
These program  transformations are hence generally applicable program transformations in
a programmer's modularity toolbox.

But what about refunctionalization in the case that there is more than one pattern-match on the
data type? It turns out that we can refunctionalize if we generalize functions to objects, that is,
if we view functions as special kinds of objects with a single ``apply`` method.

For instance, in the case of lists with two pattern-matching functions, ``nth`` and ``length``,
we can represent lists as _objects_ with two methods, one for each pattern match.

```scala mdoc
trait MyListRO[T] {
  def nth(i: Int): T
  def length: Int
}

def myEmptyListO[T]: MyListRO[T] = new MyListRO[T] {
  def nth(i: Int): T = sys.error("index out of range")
  def length: Int = 0
}

def myConsO[T](x: T,  xs: MyListRO[T]): MyListRO[T] = new MyListRO[T] {
  def nth(i: Int): T = if (i == 0) then x else xs.nth(i - 1)
  def length: Int =  1 + xs.length
}
```

With this generalization, we can defunctionalize any object type (the transformation
could then maybe be called "deobjectionalization") and we can refunctionalize any
algebraic data type, and thereby completely invert the extensibility of the program.

Recall that both transformations are _global_ and not compositional, that is, the full program must be
transformed at once.

We have glossed over some technical details that would need to be addressed to automate
these transformations and make them inverse to each other. For instance, we have not
addressed how to reverse the lambda lifting part.

Historical notes: Defunctionalization was proposed by John Reynolds in 1972 in his landmark
paper "Definitional Interpreters for Higher-Order Programming Languages".
Similar to these lecture notes, Reynolds applied defunctionalization to a CPS-transformed interpreter.
The generalization of refunctionalization to use objects instead of functions was presented
in a 2015 paper by Rendel, Trieflinger, and Ostermann entitled "Automatic Refunctionalization to a
Language with Copattern Matching: With Applications to the Expression Problem". A fully formal account
of the transformations that also addresses invertible lambda lifting and proves that the
transformations are inverses of each other can be found in the 2020 paper
"Decomposition Diversity with Symmetric Data and Codata" by Binder, Jabs, Skupin and Ostermann.
