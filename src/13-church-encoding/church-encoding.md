# Church Encodings

The content of this chapter is available as a Scala file [here.](./church-encoding.scala)

Today we shrink our language. It does not seem to be big, but today we want to  illustrate how powerful our core language,
the lambda calculus, is. Here is a shrinked version of FAE that does not even have numbers anymore. For testing purposes,
we introduce a new expression ``PrintDot`` whose semantics is to print a dot on the screen.

```scala mdoc
sealed abstract class Exp
case class Id(name: String) extends Exp
implicit def id2exp(s: String): Exp = Id(s)
case class Fun(param: String, body: Exp) extends Exp
case class Ap (funExpr: Exp, argExpr: Exp) extends Exp
case class PrintDot() extends Exp

abstract class Value // the only values are closures
type Env = Map[String, Value]
case class ClosureV(f:Fun, env:Env) extends Value
```

Notice that the only values in this language are closures. This means that there cannot be the situation anymore that we expect,
say, a number but get in fact a closure. Hence, this language has the fascinating property that  no dynamic type errors can occur.

```scala mdoc
def eval(e: Exp, env: Env) : Value = e match {
  // We give the identity function as dummy value for PrintDot
  case PrintDot() => print("."); ClosureV(Fun("x","x"), Map.empty)
  case Id(x) => env(x)
  case f@Fun(param,body) => ClosureV(f, env)
  case Ap(f,a) => eval(f,env) match {
    case ClosureV(f,closureEnv) => eval(f.body, closureEnv + (f.param -> eval(a,env)))
  }
}
```

Now we want to illustrate that we can, in principle, bootstrap a full programming language from this small core.
To do so, we use the technique of Church encoding. This means that each datum is represented by its own fold function.
Church Encoding of Booleans

Let"s" start with booleans and boolean arithmetic.

```scala mdoc:silent
val f = Fun("t", Fun("f", "f"))  // false
val t = Fun("t", Fun("f", "t"))  // true
val and = Fun("a", Fun("b", Ap(Ap("a", "b"),"a")))
val or = Fun("a", Fun("b", Ap(Ap("a", "a"), "b")))
val not = Fun("a", Fun("t", Fun("f", Ap(Ap("a","f"),"t"))))
```

We can now write our own control structures, such as ``if/then/else``

```scala mdoc:silent
val ifthenelse = Fun("cond", Fun("t", Fun("f", Ap(Ap("cond", "t"), "f"))))
```

## Church Encoding of Numbers


Let"s" now consider Numbers. We encode them as Peano numbers.  These encodings of numbers are often called _Church numbers_.

```scala mdoc:silent
val zero = Fun("s", Fun("z", "z"))
val succ = Fun("n", Fun("s", Fun("z", Ap("s", Ap(Ap("n", "s"),"z")))))
val one = Ap(succ, zero)
val two = Ap(succ, one)
val three = Ap(succ, two)
val add  = Fun("a", Fun("b", Fun("s", Fun("z", Ap(Ap("a","s"), Ap(Ap("b", "s"),"z"))))))
val mult = Fun("a", Fun("b", Fun("s", Fun("z", Ap(Ap("a", Ap("b","s")), "z")))))
val exp  = Fun("a", Fun("b", Ap(Ap("b", Ap(mult, "a")), one)))
val iszero = Fun("a", Ap(Ap("a", Fun("x", f)), t))
```

The predecessor function is more complicated (why?). We do not show it here.
For testing, here is a function that prints a unary representation of a number.

```scala mdoc:silent
val printnum = Fun("a", Ap(Ap("a", Fun("x", PrintDot())), f))
```

## Church encoding of lists

Again straightforward, except "tail", which we do not show here. It needs the same kind of trick (called "pairing trick")
as the predecessor function.

```scala mdoc:silent
val emptylist = Fun("c", Fun("e", "e"))
val cons = Fun("h", Fun("r", Fun("c", Fun("e", Ap(Ap("c", "h"), Ap(Ap("r","c"),"e"))))))
val head = Fun("l", Ap(Ap("l", Fun("h", Fun("t", "h"))), f))
```

For instance, we can multiply all numbers in a list

```scala mdoc:silent
val multlist = Fun("l", Ap(Ap("l", mult), one))
```

Here is the list 3,2,3:

```scala mdoc:silent
val list323 = Ap(Ap(cons, three), Ap(Ap(cons, two), Ap(Ap(cons,three),emptylist)))

```

```scala mdoc:silent
val test = Ap(printnum, Ap(multlist, list323))
// Calling exec should yield 18 dots before the dummy result
def exec = eval(test, Map.empty)
```

Topic for class discussion: Can we do these encodings directly in Scala or Haskell?
```
