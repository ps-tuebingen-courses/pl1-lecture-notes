import scala.language.implicitConversions

enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Mul(lhs: Exp, rhs: Exp)
  case If0(cond: Exp, thenExp: Exp, elseExp: Exp)
  case Fun(param: String, body: Exp)
  case Ap (funExpr: Exp, argExpr: Exp)

  case NewBox(e: Exp)
  case SetBox(b: Exp, e: Exp)
  case OpenBox(b: Exp)
  case Seq(e1: Exp, e2: Exp)

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)
  def wth(x: String, xdef: Exp, body: Exp): Exp = Ap(Fun(x, body), xdef)

import Exp._

abstract class Value {
  var marked: Boolean = false
}

type Env = scala.collection.mutable.Map[String, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value
case class AddressV(a: Int) extends Value

trait Store {
  def malloc(stack: List[Env], v: Value): Int
  def update(index: Int, v: Value): Unit
  def apply(index: Int): Value
}

def eval(e: Exp, stack: List[Env], store: Store): Value = e match {

  case Num(n) => NumV(n)

  case Id(x) => stack.head(x)

  case f@Fun(_, _) => ClosureV(f, stack.head)

  /* With a mutable store, we do not have to thread it according to
   * the order of evaluation any more.
   */

  case If0(cond, thenExp, elseExp)
    => eval(cond, stack, store) match {
         case NumV(0) => eval(thenExp, stack, store)
         case _       => eval(elseExp, stack, store)
       }

  /* The mutable store allows us to take advantage of Scala's
   * evaluation order and perform two pattern matchings
   * simultaneously.
   */

  case Add(l, r)
    => (eval(l, stack, store), eval(r, stack, store)) match {
         case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
         case _ => sys.error("can only add numbers")
       }

  case Mul(l, r)
    => (eval(l, stack, store), eval(r, stack, store)) match {
         case (NumV(v1), NumV(v2)) => NumV(v1 * v2)
         case _ => sys.error("can only multiply numbers")
       }

  /* A new environment should be pushed onto the stack only when
   * binding occurs. Where exactly in BCFAE do bindings happen?
   */

  case Ap(f, a)
    => eval(f, stack, store) match {
         case ClosureV(f, cEnv)
           => eval(
                f.body,
                (cEnv += (f.param -> eval(a, stack, store))) :: stack,
                store
              )
         case _ => sys.error("can only apply functions")
       }

  /* The mutable store allows us to implement Seq-expression
   * in terms of sequencing in Scala itself.
   */

  case Seq(e1, e2)
    => eval(e1, stack, store); eval(e2, stack, store)

  case NewBox(e: Exp)
    => {
         val a = store.malloc(stack, eval(e, stack, store))
         AddressV(a)
       }

  case SetBox(b: Exp, e: Exp)
    => eval(b, stack, store) match {
         case AddressV(a)
           => {
                val ev = eval(e, stack, store)
                store.update(a, ev)
                ev
              }
         case _ => sys.error("can only set boxes")
       }

  case OpenBox(b: Exp)
    => eval(b, stack, store) match {
         case AddressV(a) => store(a)
         case _ => sys.error("can only open boxes")
       }
}

class NoGCStore(size: Int) extends Store {

  val memory = new Array[Value](size)

  var nextFreeAddr: Int = 0

  def malloc(stack: List[Env], v: Value): Int = {
    val x = nextFreeAddr
    if (x >= size) sys.error("out of memory")
    nextFreeAddr += 1
    update(x, v)
    x
  }

  def update(index: Int, v: Value): Unit = memory.update(index, v)

  def apply(index: Int) = memory(index)
}

class MarkAndSweepStore(size: Int) extends Store {

  val memory = new Array[Value](size)

  var free: Int = size

  var nextFreeAddr: Int = 0

  def malloc(stack: List[Env], v: Value): Int = {
    if (free <= 0) gc(stack)
    if (free <= 0) sys.error("out of memory")

    /* Here we find the next available location in memory via a while-
     * loop. In order to avoid maintaining a list of available spaces
     * (because we are lazy), let us assume that no box created in
     * BCFAE can have an address pointing to a null memory cell (which
     * also is the case).
     *
     * If we ensure the invariant that the variable `free` has always
     * the number of free memory space, then the following loop will
     * always halt. The nontermination situation will generate an out-
     * of-memory error and the program will abort. */

    while (memory(nextFreeAddr) != null) {
      nextFreeAddr += 1
      if (nextFreeAddr == size) nextFreeAddr = 0
    }

    free -= 1
    update(nextFreeAddr, v)
    nextFreeAddr
  }

  def update(index: Int, v: Value): Unit = memory.update(index, v)

  def apply(index: Int) = memory(index)

  def allAddrInVal(v: Value): Set[Int] = v match {
    case AddressV(a)      => Set(a)
    case NumV(_)          => Set.empty
    case ClosureV(f, env) => allAddrInEnv(env)
  }

  def allAddrInEnv(env: Env): Set[Int] =
    env.values.map(allAddrInVal _).fold(Set.empty)(_ union _)

  def mark(seed: Set[Int]): Unit = {
    seed.foreach(memory(_).marked = true)
    val newAddresses = seed.flatMap(
                         ad => allAddrInVal(memory(ad))
                       ).filter(!memory(_).marked)
    if(newAddresses != Set.empty) {
      mark(newAddresses)
    }
  }

  /* What graph algorithm underlies the mark step as implemented here?
   * What potential problem could it cause in a "real" interpreter? */

  def sweep(): Unit = {
    memory.indices.foreach(
      index =>   if (memory(index) == null) {
                   /* No work needed on an empty memory cell */
                 }
                 else if (memory(index).marked) {
                   /* Reset `marked` flag for the next gc */
                   memory(index).marked = false
                 }
                 else {
                   free += 1
                   memory(index) = null
                 }
    )
  }

  def gc(stack: List[Env]): Unit = {
    println("\nSTARTING GC\nSTACK = " + stack + "\nSTORE = " + memory)
    mark(stack.map(allAddrInEnv _).fold(Set.empty)(_ union _))
    sweep()
    println("GC COMPLETE\nSTORE = " + memory +
            "\nNUMBER OF FREE SLOTS = " + free)
  }
}

val test4 = wth("makedata", Fun("x", NewBox(NewBox(NewBox("x")))),
                Seq(Ap("makedata", 1),
                Seq(Ap("makedata", 2),
                Seq(wth("s", Ap("makedata", 3), Ap("makedata", "s")),
                    Ap("makedata", 4)))))

def runTest4 = eval(test4,
                    List(scala.collection.mutable.Map.empty),
                    new MarkAndSweepStore(5))

