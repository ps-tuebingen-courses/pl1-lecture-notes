import scala.io.StdIn.readLine

def inputNumber(prompt: String): Int = {
  println(prompt)
  Integer.parseInt(readLine())
}

def progSimple = {
  println(inputNumber("First number:") + inputNumber("Second number:"))
}

// we use the return type "Nothing" for functions that will never return (normally)
def webdisplay(s: String): Nothing = {
  println(s)
  sys.error("program terminated")
}

def webread(prompt: String, continue: String): Nothing = {
  println(prompt)
  println("send input to: " + continue)
  sys.error("program terminated")
}

def program1 = webread("enter first number", "s2")
def program2(n1: Int) = webread("enter second number and remind me of previoulsy entered number " + n1, "s3")
def program3(n1: Int, n2: Int) = webdisplay("The sum of " + n1 + " and " + n2 + " is " + (n1 + n2))

val cont1 = (n: Int) => println(n + inputNumber("Second number"))

val continuations = new scala.collection.mutable.HashMap[String, Int => Nothing]()
var nextIndex: Int = 0
def getNextID = {
  nextIndex += 1
  "c" + nextIndex
}

def webread_k(prompt: String, k: Int => Nothing): Nothing = {
  val id = getNextID
  continuations += (id -> k)
  println(prompt)
  println("to continue, invoke continuation: " + id)
  sys.error("program terminated")
}

def continue(kid: String, result: Int) = continuations(kid)(result)

def webprog = webread_k("enter first number", (n) =>
                webread_k("enter second number", (m) =>
                  webdisplay("The sum of " + n + " and " + m + " is " + (n + m))))

def allCosts(itemList: List[String]): Int = itemList match {
   case Nil => 0
   case x :: rest => inputNumber("Cost of item " + x + ":") + allCosts(rest)
}

val testData: List[String] = List("banana", "apple", "orange")

def test = println("Total sum: " + allCosts(testData))

def allCosts_k(itemList: List[String], k: Int => Nothing): Nothing = itemList match {
   case Nil => k(0)
   case x :: rest => webread_k("Cost of item " + x + ":", n => allCosts_k(rest, m => k(m + n)))
}

def testweb = allCosts_k(testData, m => webdisplay("Total sum: " + m))

def map[S, T](c: List[S], f: S => T): List[T] = c match {
  case Nil => Nil
  case x :: rest => f(x) :: map(rest, f)
}

def allCosts2(itemList: List[String]): Int =
  map(itemList, (x: String) => inputNumber("Cost of item " + x + ":")).sum

def map_k[S, T](c: List[S], f: (S, T => Nothing) => Nothing, k: List[T] => Nothing): Nothing = c match {
   case Nil => k(Nil)
   case x :: rest => f(x, t => map_k(rest, f, (tr: List[T]) => k(t :: tr)))
}

def allCosts2_k(itemList: List[String], k: Int => Nothing): Nothing =
   map_k(itemList,
         (x: String, k2: Int => Nothing) =>
           webread_k("Cost of item " + x + ":", k2),
         (l: List[Int]) => k(l.sum))

