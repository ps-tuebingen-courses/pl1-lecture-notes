import scala.language.higherKinds
import scala.language.reflectiveCalls

def f(n: Int) : String = "x"
def g(x: String) : Boolean = x == "x"
def h(b: Boolean) : Int = if (b) 27 else sys.error("error")

def clientCode = h(!g(f(27)+"z"))

def fOp(n: Int) : Option[String] = if (n < 100) Some("x") else None
def gOp(x: String) : Option[Boolean] = Some(x == "x")
def hOp(b: Boolean) : Option[Int] = if (b) Some(27) else None

def clientCodeOp =
  fOp(27) match {
    case Some(x) => gOp(x+"z") match {
            case Some(y) => hOp(!y)
            case None => None
      }
    case None => None
  }

def bindOption[A,B](a: Option[A], f: A => Option[B]) : Option[B] = a match {
  case Some(x) => f(x)
  case None => None
}

def clientCodeOpBind =
  bindOption(fOp(27), (x:String) =>
    bindOption(gOp(x+"z"), (y:Boolean) =>
      hOp(!y)))

    def clientCode =
      f(27) bind ((x: String) =>
      g(x+"z") bind  ((y: Boolean) =>
      !y))

def clientCode2Op =
  bindOption(fOp(27), ((x: String) =>
  bindOption(gOp(x+"z"), ((y: Boolean) =>
  Some(!y)))))

def unit[A](x: A) : Option[A] = Some(x)

def clientCode2OpUnit =
  bindOption(fOp(27), ((x: String) =>
  bindOption(gOp(x+"z"), ((y: Boolean) =>
  unit(!y)))))

trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def bind[A,B](m: M[A], f: A => M[B]): M[B]
  // The "monad laws":
  // 1) "unit" acts as a kind of neutral element of "bind", that is:
  //    1a) bind(unit(x),f) == f(x) and
  //    1b) bind(x, y => unit(y)) == x
  // 2) Bind enjoys an associative property
  //     bind(bind(x,f),g) == bind(x, y => bind(f(y),g))
}

def clientCode2Op(m: Monad[Option]) =
  m.bind(fOp(27), (x: String) =>
  m.bind(gOp(x+"z"), (y: Boolean) =>
  m.unit(!y)))

def fM[M[_]](n: Int)(using m: Monad[M]) : M[String] = sys.error("not implemented")
def gM[M[_]](x: String)(using m: Monad[M]) : M[Boolean] = sys.error("not implemented")
def hM[M[_]](b: Boolean)(using m: Monad[M]) : M[Int] = sys.error("not implemented")

def clientCode2[M[_]](using m: Monad[M]) =
  m.bind(fM(27), (x: String) =>
  m.bind(gM(x+"z"), (y: Boolean) =>
  m.unit(!y)))

  val l = List(List(1,2), List(3,4))
  assert( (for { x <- l; y <- x } yield y+1) == List(2,3,4,5))

  assert(l.flatMap(x => x.map(y => y+1)) == List(2,3,4,5))

extension [A, M[_]](m: M[A])(using mm: Monad[M])
  def map[B](f: A => B): M[B] = mm.bind(m, (x: A) => mm.unit(f(x)))
  def flatMap[B](f: A => M[B]): M[B] = mm.bind(m, f)

def fOp(n: Int) : Option[String] = if (n < 100) Some("x") else None
def gOp(x: String) : Option[Boolean] = Some(x == "x")
def hOp(b: Boolean) : Option[Int] = if (b) Some(27) else None

def clientCode2Op(m: Monad[Option]) =
  m.bind(fOp(27), (x: String) =>
  m.bind(gOp(x+"z"), (y: Boolean) =>
  m.unit(!y)))

def clientCode2OpFor(using m: Monad[Option]) =
  for {
    x <- fOp(27)
    y <- gOp(x+"z")
  } yield !y

object OptionMonad extends Monad[Option] {
  override def bind[A,B](a: Option[A], f: A => Option[B]) : Option[B] =
    a match {
      case Some(x) => f(x)
      case None => None
    }
  override def unit[A](a: A) = Some(a)
}

def v : Option[Boolean] = clientCode2Op(OptionMonad)

def fmap[M[_],A,B](f: A => B)(using m: Monad[M]): M[A] => M[B] = a => m.bind(a,(x:A) => m.unit(f(x)))

def sequence[M[_],A](l: List[M[A]])(using m: Monad[M]) : M[List[A]] = l match {
  case x :: xs => m.bind(x, (y: A) =>
    m.bind(sequence(xs), (ys : List[A]) =>
      m.unit(y :: ys)))
  case Nil => m.unit(List.empty)
}

def mapM[M[_],A,B](f : A => M[B], l: List[A])(using m: Monad[M]) : M[List[B]] =
  sequence(l.map(f))

def join[M[_],A](x : M[M[A]])(using m: Monad[M]) : M[A] = m.bind(x, (y : M[A]) => y)

type Id[X] = X   
object IdentityMonad extends Monad[Id] {
  def bind[A,B](x: A, f: A => B) : B = f(x)
  def unit[A](a: A) : A = a
}

trait ReaderMonad[R] extends Monad[[A] =>> R => A] {
  override def bind[A,B](x: R => A, f: A => R => B) : R => B = r => f(x(r))(r) // pass the "environment" r into both computations
  override def unit[A](a: A) : R => A = (_) => a
}

def fRead(n: Int) : Int => String  = sys.error("not implemented")
def gRead(x: String) : Int => Boolean  = sys.error("not implemented")
def hRead(b: Boolean) : Int => Int = sys.error("not implemented")

  def clientCode = h(!g(f(27)+"z"))
becomes :
def clientCodeRead(env: Int) = hRead(!gRead(fRead(27)(env)+"z")(env))(env)

def clientCode2Read(using m: ReaderMonad[Int]) =
  m.bind(fRead(27), ((x: String) =>
  m.bind(gRead(x+"z"), ((y: Boolean) =>
  m.unit(!y)))))

/* this code does not work in older versions of Scala */
def clientCode2ReadFor(using m: ReaderMonad[Int]) =
    for {
      x <- fRead(27)
      y <- gRead(x+"z")
    } yield !y

trait StateMonad[S] extends Monad[[A] =>> S => (A,S)] {
  override def bind[A,B](x: S => (A,S), f: A => S => (B,S)) : S => (B,S) =
    s => x(s) match { case (y,s2) => f(y)(s2) } // thread the state through the computations
  override def unit[A](a: A) : S => (A,S) = s => (a,s)
}

def fState(n: Int) : Int => (String,Int)  = sys.error("not implemented")
def gState(x: String) : Int => (Boolean,Int)  = sys.error("not implemented")
def hState(b: Boolean) : Int => (Int,Int) = sys.error("not implemented")

def clientCodeState(s: Int) =
  fState(27)(s) match {
    case (x,s2) => gState(x+"z")(s2) match {
      case (y,s3) => hState(!y)(s3) }}

def clientCode2State(using m: StateMonad[Int]) =
  m.bind(fState(27), ((x: String) =>
  m.bind(gState(x+"z"), ((y: Boolean) =>
  m.unit(!y)))))

// This still does not work in Scala 3.
//def clientCode2StateFor(using m: StateMonad[Int]) =
//  for {
//    x <- fState(27)
//    y <- gState(x+"z")
//  } yield !y

object ListMonad extends Monad[List] {
  override def bind[A,B](x: List[A], f: A => List[B]) : List[B] = x.flatMap(f) // apply f to each element, concatenate the resulting lists
  override def unit[A](a: A) = List(a)
}
__Example__: Assume that our API functions return lists of results, and our client code must exercise the combination of all possible answers.
def fList(n: Int) : List[String] = sys.error("not implemented")
def gList(x: String) : List[Boolean] = sys.error("not implemented")
def hList(b: Boolean) : List[Int] = sys.error("not implemented")

def clientCodeList =
  fList(27).map(x => gList(x+"z")).flatten.map(y => hList(!y)).flatten

def clientCode2List = {
  given Monad[List] = ListMonad
  for {
    x <- fList(27)
    y <- gList(x+"z")
  } yield !y
}

trait ContinuationMonad[R] extends Monad[[A] =>> (A => R) => R] {
  type Cont[X] = (X => R) => R

  override def bind[A,B](x: Cont[A], f: A => Cont[B]) : Cont[B] =
     k => x( a => f(a)(k)) // construct continuation for x that calls f with the result of x               
  override def unit[A](a: A) : Cont[A] = k => k(a)

  // callcc is like letcc; the difference is that letcc binds a name, whereas callcc expects a function as argument
  // That means that letcc(k,...) is expressed as callcc( k => ...).
  def callcc[A,B](f: (A => Cont[B]) => Cont[A]   ) : Cont[A] = k => f( (a:A) => (_:B=>R) => k(a))(k)
}

def fCPS[R](n: Int) : (String => R) => R = sys.error("not implemented")
def gCPS[R](x: String) : (Boolean => R) => R = sys.error("not implemented")
def hCPS[R](b: Boolean) : (Int => R) => R = sys.error("not implemented")

def clientCodeCPS[R]: (Int => R) => R =
  k => fCPS(27)( (x: String) => gCPS(x+"z")( (y: Boolean) =>  hCPS(!y)(k)))

def clientCode2CPS[R](using m: ContinuationMonad[R]) =
  m.bind(fCPS(27), ((x: String) =>
  m.bind(gCPS(x+"z"), ((y: Boolean) =>
  m.unit(!y)))))

/** this still does not work in Scala 3 */
//def clientCode2CPSFor[R](using m: ContinuationMonad[R]) =
//  for {
//    x <- fCPS(27)
//    y <- gCPS(x+"z")
//  } yield !y

// unfortunately we can, again, not use for-comprehension syntax
def ex123[R](using m: ContinuationMonad[R])  = {
  m.bind(
    m.bind(m.unit(2), (two:Int) =>
      m.bind(m.unit(3), (three:Int) => m.unit(two+three))),
    (five:Int) => m.unit(1+five))
}

def runEx123 = ex123(using new ContinuationMonad[Int]{})(x=>x)

def excallcc[R](using m: ContinuationMonad[R])  = {
  m.bind(
    m.bind(m.unit(2),
           (two:Int) => m.callcc[Int,Int]( k => m.bind(k(3), (three:Int) => m.unit(two+three)))),
    (five:Int) => m.unit(1+five))
}
def runExcallcc = excallcc(using new ContinuationMonad[Int]{})(x=>x)

def mapM[M[_],A,B](x: List[A], f: A => M[B])(using m: Monad[M]) : M[List[B]] = sequence(x.map(f))

type OptionT[M[_]] = { type x[A] = M[Option[A]] }


class OptionTMonad[M[_]](val m : Monad[M]) extends Monad[OptionT[M]#x] {

  override def bind[A,B](x : M[Option[A]], f: A => M[Option[B]]) : M[Option[B]] =
    m.bind(x, (z: Option[A]) => z match { case Some(y) => f(y)
                                          case None => m.unit(None) })

  override def unit[A](a: A) = m.unit(Some(a))

  def lift[A](x: M[A]) : M[Option[A]] = m.bind(x, (a: A) => m.unit(Some(a)))                                   
}

val ListOptionM = new OptionTMonad(ListMonad)
```scala mdoc
