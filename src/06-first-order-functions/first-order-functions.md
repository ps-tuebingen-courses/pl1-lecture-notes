# First order functions

The content of this chapter is available as a Scala file [here.](./first-order-functions.scala)

```scala mdoc:invisible
import scala.language.implicitConversions
```

## First-Order Functions

In the last lecture we have seen how we can give commonly occuring (sub)expressions a name via the `With` construct. Often, however,
we can identify _patterns_ of expressions that occur in many places, such as ``5*5/2``, ``7*7/2`` and ``3*3/2``, the common pattern
being ``x*x/2``. In this case, the abstraction capabilities of `With` are not sufficient.
One way to enable more powerful abstractions are _functions_. Depending on the context of use and the interaction with other language
features (such as imperative features or objects), functions are also sometimes called _procedures_ or _methods_.
Here we consider so-called first-order functions, that - unlike higher-order functions - are not expressions and can hence not be passed
to or be returned from other functions. First-order functions are simply called by name.
To introduce first-order functions, we need two new things: The possibility to define functions, and the possibility to call functions.
A call to a function is an expression, whereas functions are defined separately. Functions can have an arbitrary number of arguments.
The following definitions are the language we have analyzed so far together with the new language constructs for first-order functions:

```scala mdoc
object Syntax {
  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Mul(lhs: Exp, rhs: Exp) extends Exp
  case class Id(x: String) extends Exp
  case class With(x: String, xdef: Exp, body: Exp) extends Exp
  /** We use implicits again to make example programs less verbose. */
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def string2exp(x: String): Exp = Id(x)


  /** The new language constructs for first-order functions: */
  case class Call(f: String, args: List[Exp]) extends Exp // functions are called by name

  case class FunDef(args: List[String], body: Exp)
  type Funs = Map[String,FunDef]
}
import Syntax._
```

A function has a number of formal args and a body. Note that a first-order function also
has a name. To make the invariant that there can only be one function for each
name explicit, we have stored functions in the form of a map from function names to
`FunDefs` above.

The substitution for the new language is a straightforward extension of the former one.

```scala mdoc
def subst(e: Exp,i: String,v: Num) : Exp =  e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l,r) => Add( subst(l,i,v), subst(r,i,v))
    case Mul(l,r) => Mul( subst(l,i,v), subst(r,i,v))
    case With(x,xdef,body) => With(x,
                                   subst(xdef,i,v),
                                   if (x == i) body else subst(body,i,v))
    case Call(f,args) => Call(f, args.map(subst(_,i,v)))
}
```

We will first study a "reference interpreter" based on substitution.
We pass the map of functions as an additional parameter.

```scala mdoc
def eval(funs: Funs, e: Exp) : Int = e match {
  case Num(n) => n
  case Id(x) => sys.error("unbound identifier: " + x)
  case Add(l,r) => eval(funs,l) + eval(funs,r)
  case Mul(l,r) => eval(funs,l) * eval(funs,r)
  case With(x, xdef, body) => eval(funs,subst(body,x,Num(eval(funs,xdef))))
  case Call(f,args) => {
     val fd = funs(f) // lookup function definition
     val vargs = args.map( eval(funs,_)) // evaluate function arguments
     if (fd.args.size != vargs.size) sys.error("number of paramters in call to " + f + " does not match")
     // We construct the function body to be evaluated by subsequently substituting all formal
     // arguments with their respective argument values.
     // If we have only a single argument "fd.arg" and a single argument value "varg",
     // the next line of code is equivalent to:
     // val substbody = subst(fd.body, fd.arg, Num(varg))
     val substbody = fd.args.zip(vargs).foldLeft(fd.body)( (b,av) => subst(b,av._1,Num(av._2)) )
     eval(funs,substbody)
  }
}
```

Is the extension really so straightforward?  It can be seen in the last line of our
definition for ``subst`` that variable substitution deliberately ignores the function
name ``f``. The substitution for ``f`` instead is handled separately inside ``eval``.
We say in this case that function names and variable names live in different "name spaces".
An alternative would be to have them share one namespace. As an exercise, think about how
to support a common namespace for function names and variable names.

A test case:

```scala mdoc:silent
val someFuns = Map( "adder" -> FunDef(List("a","b"), Add("a","b")),
                 "doubleadder" -> FunDef(List("a","x"), Add(Call("adder", List("a",5)),Call("adder", List("x",7)))))
```

```scala mdoc
val callSomeFuns = eval(someFuns,Call("doubleadder",List(2,3)))
assert( callSomeFuns == 17)
```


### The scope of function definitions:

As can be seen in the example above, each function can "see" the other functions. We say that in this language functions have a _global scope_.
Exercise: Can a function also invoke itself? Is this useful?
We will now study an environment-based version of the interpreter. To motivate environments, consider the following sample program:

```scala mdoc:silent
val testProg = With("x", 1, With("y", 2, With("z", 3, Add("x",Add("y","z")))))
```

When considering the ``With`` case of the interpreter, the interpreter will subsequently produce and evaluate the following intermediate expressions:

```scala mdoc:silent
val testProgAfterOneStep     = With("y", 2, With("z", 3, Add(1,Add("y","z"))))
val testProgAfterTwoSteps    = With("z", 3, Add(1,Add(2,"z")))
val testProgAfterThreeSteps  = Add(1,Add(2,3))
```

At this point only pure arithmetic is left. But we see that the interpreter had to apply subsitution three times. In general, if the
program size is n, then the interpreter may perform up to O(n) substitutions, each of which takes O(n) time. This quadratic complexity
seems rather wasteful. Can we do better?
We can avoid the redundancy by deferring the substitutions until they are really needed. Concretely, we define a repository of deferred
substitutions, called _environment_. It tells us which identifiers are supposed to be eventually substituted by which value. This idea
is captured in the following type definition:

```scala mdoc
type Env = Map[String,Int]
```

Initially, we have no substitutions to perform, so the repository is empty. Every time we encounter a construct (a `With` or an application `Call`)
that requires substitution, we augment the repository with one more entry, recording the identifier’s name and the value (if eager) or
expression (if lazy) it should eventually be substituted with. We continue to evaluate without actually performing the substitution.
This strategy breaks a key invariant we had established earlier, which is that any identifier the interpreter could encounter must be
free, for had it been bound, it would have already been substituted.  Now that we’re longer using the substitution-based model, we may
encounter bound identifiers during interpretation.  How do we handle them?  We must substitute them by consulting the repository.

```scala mdoc
def evalWithEnv(funs: Funs, env: Env, e: Exp) : Int = e match {
  case Num(n) => n
  case Id(x) => env(x) // look up in repository of deferred substitutions
  case Add(l,r) => evalWithEnv(funs,env,l) + evalWithEnv(funs,env,r)
  case Mul(l,r) => evalWithEnv(funs,env,l) * evalWithEnv(funs,env,r)
  case With(x, xdef, body) => evalWithEnv(funs,env + ((x,evalWithEnv(funs,env,xdef))),body)
  case Call(f,args) => {
     val fd = funs(f) // lookup function definition
     val vargs = args.map(evalWithEnv(funs,env,_)) // evaluate function arguments
     if (fd.args.size != vargs.size) sys.error("number of paramters in call to " + f + " does not match")
     // We construct the environment by associating each formal argument to its actual value
     val newenv = Map() ++ fd.args.zip(vargs)
     evalWithEnv(funs,newenv,fd.body)
  }
}

val evalEnvSomeFuns = evalWithEnv(someFuns,Map.empty, Call("doubleadder",List(2,3)))
assert( evalEnvSomeFuns == 17)
```

In the interpreter above, we have extended the empty environment when constructing ``newenv``. A conceivable alternative is to
extend ``env`` instead, like so:

```scala mdoc
def evalDynScope(funs: Funs, env: Env, e: Exp) : Int = e match {
  case Num(n) => n
  case Id(x) => env(x)
  case Add(l,r) => evalDynScope(funs,env,l) + evalDynScope(funs,env,r)
  case Mul(l,r) => evalDynScope(funs,env,l) * evalDynScope(funs,env,r)
  case With(x, xdef, body) => evalDynScope(funs,env + ((x,evalDynScope(funs,env,xdef))),body)
  case Call(f,args) => {
     val fd = funs(f)
     val vargs = args.map(evalDynScope(funs,env,_))
     if (fd.args.size != vargs.size) sys.error("number of paramters in call to "+ f + " does not match")
     val newenv = env ++ fd.args.zip(vargs) // extending env instead of Map() !!
     evalDynScope(funs,newenv,fd.body)
  }
}

val evalDynSomeFuns = evalDynScope(someFuns,Map.empty, Call("doubleadder",List(2,3)))
assert( evalDynSomeFuns == 17)
```

Does this make a difference? Yes, it does. Here is an example:

```scala mdoc:silent
val funnyFun = Map( "funny" -> FunDef(List("a"), Add("a","b")))
```

```scala mdoc
val evalDynFunnyFun = evalDynScope(funnyFun, Map.empty, With("b", 3, Call("funny",List(4))))
assert(evalDynFunnyFun == 7)
```

Obviously this interpreter is "buggy" in the sense that it does not agree with the substitution-based interpreter. But is this semantics reasonable?
Let's introduce some terminology to make the discussion simpler:

> **Definition (Static Scope)**:
> In a language with static scope, the scope of an identifier’s binding is a syntactically delimited region.
> A typical region would be the body of a function or other binding construct.

> **Definition (Dynamic Scope)**: In a language with dynamic scope, the scope of an identifier’s binding is the entire remainder of the
> execution during which that binding is in effect.


We see that ``eval`` and ``evalWithEnv`` give our language static scoping, whereas `evalDynScope` gives our language dynamic scoping.
Armed with this terminology, we claim that dynamic scope is entirely unreasonable. The problem is that we simply cannot determine what
the value of a program will be without knowing everything about its execution history. If a function `f` were invoked by some
sequence of other functions that did not bind a value for some parameter of `f`, then that particular application of `f` would result in an error, even though a
previous application of `f` in the very same program’s execution may have completed successfully! In other words, simply by looking at the
source text of `f`, it would be impossible to determine one of the most rudimentary properties of a program: whether or not a given
identifier was bound. You can only imagine the mayhem this would cause in a large software system, especially with multiple developers
and complex ﬂows of control. We will therefore regard dynamic scope as an error. That said, there are facets of dynamic binding
that are quite useful. For instance, exception handlers are typically dynamically scoped: A thrown exception is dispatched to the
most recently encountered active exception handler for that exception type. 
