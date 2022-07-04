# Defunctionalization

The content of this chapter is available as a Scala file [here.](./defunctionalization.scala)


The material in these notes is based on: John C. Reynolds: Definitional Interpreters for Higher-Order Programming Languages.
Higher-Order and Symbolic Computation 11(4): 363-397 (1998)

In the discussion of syntactic interpretation vs. meta interpretation we have learned that we only learn something about (and control)
a language feature if we choose syntactic interpretation.

Today we want to discuss techniques with which we can make our interpreter so syntactic that it corresponds to an abstract machine:
A machine with a (possibly infinite) set of states and a simple transition relation between the states. We already know the technique
with which we can take control over the call stack management: _CPS transformation_. After CPS-transforming the interpreter, we do not
rely on the order of evaluation and call stack management of the meta language anymore.  We replicate its definition here:

```scala mdoc
enum Exp:
  case Num(n : Int)
  case Id(name : String)
  case Add(lhs : Exp, rhs : Exp)
  case Fun(param : String, body : Exp)
  case Ap (funExpr : Exp, argExpr : Exp)
import Exp._

implicit def num2exp(n: Int) : Exp = Num(n)
implicit def id2exp(s: String) : Exp = Id(s)

sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n : Int) extends Value
case class ClosureV(f : Fun, env : Env) extends Value

object CPSTransformed {
  def eval[T](e : Exp, env : Env, k : Value => T) : T = e match {
    case Num(n : Int) => k(NumV(n))
    case Id(x) => k(env(x))
    case Add(l, r) =>
      eval( l, env
          , lv => eval( r, env
                      , rv => (lv, rv) match {
                          case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
                          case _ => sys.error("can only add numbers")
                        } ) )
    case f@Fun(param, body) => k(ClosureV(f, env))
    case Ap(f, a) =>
      eval( f, env
          , cl => cl match {
              case ClosureV(f, closureEnv) =>
                eval( a, env
                    , av => eval(f.body, closureEnv + (f.param -> av), k) )
              case _ => sys.error("can only apply functions")
            } )
  }
}
```

However, the CPS-transformed interpreter still uses high-level features of the meta-language, most notably first-class functions.
We will now introduce one transformation that can be used to transform a function using higher-order functions into one using only
first-order functions.  It is general program transformation technique, not restricted only to interpreters.

## Lambda Lifting

The first of these techniques is _lambda lifting_. The goal of lambda lifting is to turn local functions into top-level functions.
That is, all "lambdas" only occur at the top-level. Variables in the local environment that are normally stored in the function's
closure are instead passed as parameters to the top-level function. Lambda lifting is accomplished by the following steps:

 1. Invent a new and unique name for each function that is not a top-level function.
 2. Create a function with this name. Its body is the body of the former local function. Such a function will contain free variables.
 3. Add a parameter to so-obtained top-level function for each free variable in its body.
    Thus it becomes a higher-order function that returns a function when passed these arguments.
 4. Replace the local function by a call to the new top-level function and pass the corresponding local context via the arguments
    created in step 3.

__Example__: Let's lambda-lift the functions ``y => y + n`` and ``y => y*n`` in

```scala mdoc
def map(f : Int => Int, xs : List[Int]) : List[Int] = xs match {
  case Nil => Nil
  case (x :: xs) => f(x) :: map(f, xs)
}

def addAndMultNToList(n : Int, xs : List[Int]) = map(y => y * n, map(y => y + n, xs))
```

We create two new top-level functions. Let's call them ``f`` and ``g`` Their bodies are respectively ``y => y + n`` and ``y => y * n``.
We add a parameter for each free variable. In the example, the free variable is ``n`` in both cases:

```scala mdoc
def fLam(n : Int) = (y : Int) => y + n
def gLam(n : Int) = (y : Int) => y * n
```

or shorter:

```scala mdoc
def f(n : Int)(y : Int) = y + n
def g(n : Int)(y : Int) = y * n
```

The local function can now be replaced by a call to the new global function.

```scala mdoc
def addAndMultNToListLifted(n : Int, xs : List[Int]) = map(g(n)(_), map(f(n)(_), xs))
```

Let's now perform the same technique to the CPS-transformed interpreter given above. It contains local functions in four places:
two in the ``Add`` branch and two in the ``Ap`` branch. We call the corresponding top-level functions, from left to right,
``addc1``, ``addc2``, ``ApC1`` and ``ApC2``.

An interesting novelty in the interpreter is that some local functions (corresponding to ``addc1`` and ``ApC1``) create local
functions themselves. This means that ``addc1`` must call ``addc2`` and ``ApC1`` must call ``ApC2``. The rest of the transformation
is a straightforward application of the transformation steps described above:

```scala mdoc
object LambdaLifted {
  def addc1[T](r : Exp, env : Env, k : Value => T)(lv : Value) = eval(r, env, addc2(lv, k))

  def addc2[T](lv : Value, k : Value => T)(rv : Value) = (lv, rv) match {
    case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
    case _ => sys.error("can only add numbers")
  }

  def ApC1[T](a : Exp, env : Env, k : Value => T)(cl : Value) = cl match {
    case ClosureV(f, closureEnv) => eval(a, env, ApC2(f, closureEnv, k))
    case _ => sys.error("can only apply functions")
  }

  def ApC2[T](f : Fun, closureEnv : Env, k : Value => T)(av : Value) = eval(f.body, closureEnv + (f.param -> av), k)

  def eval[T](e : Exp, env : Env, k : Value => T) : T = e match {
    case Num(n : Int) => k(NumV(n))
    case Id(x) => k(env(x))
    case Add(l, r) => eval(l, env, addc1(r, env, k))
    case f@Fun(param, body) => k(ClosureV(f, env))
    case Ap(f, a) => eval(f, env, ApC1(a, env, k))
  }
}
```


The lambda-lifted interpreter contains no local functions anymore, but it still contains higher-order functions, since `addc1` etc.
return functions that are passed as parameters to other functions.

## Defunctionalization

_Defunctionalization_ is a program transformation technique that turns higher-order programs that have already been lambda-lifted
into first-order programs that contain no higher-order functions anymore.  Any program contains only finitely many function definitions.
The idea of defunctionalization is to assign a unique identifier to each of these function definitions. The function ``dispatch`` then
happens in a function ``apply``, which receives the identifier corresponding to a function definition and dispatches the identifier
to the right function body. Every function application within the program is then replaced by a call to the ``apply`` function with
the function identifier as the first argument.

In addition to the unique identifier, the ``apply`` function also needs bindings for the free variables in the function body.
Hence we need to store the values for these free variables along with the unique identifier. Finally, the ``apply`` function needs
to know about the argument to the function. These become additional parameters of the ``apply`` function.
Let's illustrate defunctionalization in the ``addAndMultNToList`` example from above.

```scala mdoc
sealed abstract class FunctionValue
case class F(n : Int) extends FunctionValue
case class G(n : Int) extends FunctionValue

def apply(f : FunctionValue, y : Int) : Int = f match {
  case F(n) => y + n
  case G(n) => y * n
}

def map(f : FunctionValue, xs : List[Int]) : List[Int] = xs match {
  case Nil => Nil
  case (x :: xs) => apply(f, x) :: map(f, xs)
}

def addAndMultNToListDefun(n : Int, xs : List[Int]) = map(G(n), map(F(n), xs))
```

Let's now apply defunctionalization to our CPS-transformed interpreter:

```scala mdoc
object Defunctionalized {

  sealed abstract class FunctionValue[T]
  case class AddC1[T](r : Exp, env : Env, k : FunctionValue[T]) extends FunctionValue[T]
  case class AddC2[T](lv : Value, k : FunctionValue[T]) extends FunctionValue[T]
  case class ApC1[T](a : Exp, env : Env, k : FunctionValue[T]) extends FunctionValue[T]
  case class ApC2[T](f : Fun, closureEnv : Env, k : FunctionValue[T]) extends FunctionValue[T]

  def apply[T](fv : FunctionValue[T], v : Value) : T  = fv match {
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

  def eval[T](e : Exp, env : Env, k : FunctionValue[T]) : T = e match {
    case Num(n : Int) => apply(k, NumV(n))
    case Id(x) => apply(k, env(x))
    case Add(l, r) => eval(l, env, AddC1(r, env, k))
    case f@Fun(param, body) => apply(k, ClosureV(f, env))
    case Ap(f, a) => eval(f, env, ApC1(a, env, k))
  }
}
```

This interpreter can be seen as an abstract machine. The state space of the abstract machine is
(``Exp`` x ``Env`` x ``FunctionValue``) U (``FunctionValue`` x ``Value``), where "x" stands for cross product and "U" stands for set union.
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
the meta language (Scala)

```scala mdoc
object AbstractMachine {
  sealed abstract class FunctionValue[T]
  case class AddC1[T](r : Exp, env : Env, k : FunctionValue[T]) extends FunctionValue[T]
  case class AddC2[T](lv : Value, k : FunctionValue[T]) extends FunctionValue[T]
  case class ApC1[T](a : Exp, env : Env, k : FunctionValue[T]) extends FunctionValue[T]
  case class ApC2[T](f : Fun, closureEnv : Env, k : FunctionValue[T]) extends FunctionValue[T]
  case class IdentityFV() extends FunctionValue[Value]

  sealed abstract class MachineState[T]
  case class EvalState[T](e: Exp, env: Env, fv: FunctionValue[T]) extends MachineState[T]
  case class ApplyState[T](fv: FunctionValue[T], v: Value) extends MachineState[T]
  case class Done(v: Value) extends MachineState[Value]

  def transition[T](s: MachineState[T]) : MachineState[T] =  
    s match {
      case EvalState(e,env,k) => transitionEval(e,env,k)
      case ApplyState(fv,v) => transitionApply(fv,v)
      case Done(v) => sys.error("already done")
    }

  def transitionEval[T](e: Exp, env: Env, k: FunctionValue[T]) : MachineState[T] = e match {
    case Num(n: Int) => ApplyState(k, NumV(n))
    case Id(x) => ApplyState(k,env(x))
    case Add(l,r) =>
      EvalState(l, env,AddC1(r,env,k))
    case f@Fun(param,body) => ApplyState(k,ClosureV(f,env))         
    case Ap(f,a) =>  EvalState(f, env, ApC1(a,env,k))
   }

   def transitionApply[T](fv: FunctionValue[T], v: Value) : MachineState[T] = fv match {
    case IdentityFV() => Done(v)
    case AddC1(r,env,k) => EvalState(r,env,AddC2(v,k))
    case AddC2(lv,k) => (lv,v) match {
          case (NumV(v1),NumV(v2)) => ApplyState(k, NumV(v1+v2))
          case _ => sys.error("can only add numbers")
         }
    case ApC1(a,env,k) => v match {
        case ClosureV(f, closureEnv) => EvalState(a, env, ApC2(f,closureEnv,k))
        case _ => sys.error("can only apply functions")
    }
    case ApC2(f,closureEnv,k) => EvalState(f.body, closureEnv + (f.param -> v),k)
  }
}
import AbstractMachine._
```
Now let's try this out with a concrete example and look at the tract of transitions

```scala mdoc
val test = Ap(Fun("x",Add("x",1)),5)  
val initMS : MachineState[Value] = EvalState(test,Map.empty,IdentityFV())
val s1 = transition(initMS)//= EvalState(Fun(x,Add(Id(x),Num(1))),Map(),ApC1(Num(5),Map(),IdentityFV()))
val s2 = transition(s1)   // = ApplyState(ApC1(Num(5),Map(),IdentityFV()),ClosureV(Fun(x,Add(Id(x),Num(1))),Map()))
val s3 = transition(s2)   // = EvalState(Num(5),Map(),ApC2(Fun(x,Add(Id(x),Num(1))),Map(),IdentityFV()))
val s4 = transition(s3)   // = ApplyState(ApC2(Fun(x,Add(Id(x),Num(1))),Map(),IdentityFV()),NumV(5))
val s5 = transition(s4)   // = EvalState(Add(Id(x),Num(1)),Map(x -> NumV(5)),IdentityFV())
val s6 = transition(s5)   // = EvalState(Id(x),Map(x -> NumV(5)),AddC1(Num(1),Map(x -> NumV(5)),IdentityFV()))
val s7 = transition(s6)   // = ApplyState(AddC1(Num(1),Map(x -> NumV(5)),IdentityFV()),NumV(5))
val s8 = transition(s7)   // = EvalState(Num(1),Map(x -> NumV(5)),AddC2(NumV(5),IdentityFV()))
val s9 = transition(s8)   // = ApplyState(AddC2(NumV(5),IdentityFV()),NumV(1))
val s10 = transition(s9)  // = ApplyState(IdentityFV(),NumV(6))
val s11 = transition(s10) // = Done(NumV(6))
```
We can also automate this into a function that collects the list of all states.

```scala mdoc
def evalMachine(e: Exp) : List[MachineState[Value]] =
{
  val initMS : MachineState[Value] = EvalState(e,Map.empty,IdentityFV())
  List.unfold(initMS)({ case Done(v) => None
                        case s => { val s2 = transition(s); Some((s,s2))}})
}

val q = evalMachine(test)
```
