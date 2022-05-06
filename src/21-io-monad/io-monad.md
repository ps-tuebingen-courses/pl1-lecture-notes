# IO Monad

The content of this chapter is available as a Scala file [here.](./io-monad.scala)

```scala mdoc
trait IOMonad {
  type IO[_]
  def unit[A](a: A): IO[A]
  def bind[A,B](m: IO[A], f: A => IO[B]): IO[B]
  def printString(s: String) : IO[Unit]
  def inputString : IO[String]

  def performIO[A](action: IO[A]) : A
}

val iomonad : IOMonad = new IOMonad {
  type World = String
  type IO[A] = World => (A,World)
  def unit[A](a: A): IO[A] = w => (a,w)
  def bind[A,B](m: IO[A], f: A => IO[B]): IO[B] = w => m(w) match { case (a,w2) => f(a)(w2) }
  def printString(s: String) : IO[Unit] = w => { println(s); ((),w +s+" was printed and then ...\n") }
  def inputString : IO[String] = w => { val input = scala.io.StdIn.readLine(); (input, w + input+" was entered and then ...\n") }

  def performIO[A](action: IO[A]) : A = action("The world in which nothing has happened yet, but then ...\n") match {
    case (a,w) => println("Peformed all actions. The world in which all this happened is: \n"+w); a }
}

def someIOActions(implicit m: IOMonad) : m.IO[Unit] =
  m.bind(m.printString("Enter your first name:"), (_:Unit) =>
  m.bind(m.inputString, (firstName : String) =>
  m.bind(m.printString("Enter your last name:"), (_:Unit) =>
  m.bind(m.inputString, (lastName: String) =>
  m.printString("Hello, "+firstName + " " + lastName + "!")))))

def test = iomonad.performIO(someIOActions(iomonad))
```
