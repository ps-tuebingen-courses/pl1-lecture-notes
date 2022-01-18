# Scala Basics

```scala
// val für Konstanten
// def für Methoden
// var für Variablen

abstract class Person(val name: String) {
  // String name
  // val name: String

  def sayHello() = {
    print("hello, " + name)
  }
}

trait ByeBye {
  def sayGoodbye = {
    print("bye")
  }
}

class Tillmann(name: String) extends Person(name) with ByeBye {
  override def sayHello = {
    print("hi")
  }
}

object Me extends Tillmann("Tillmann")





trait UniPerson
case class Student(val matrikelnummer: Int) extends UniPerson
case class Professor(val fachgebiet: String) extends UniPerson

object PatternMatching {
  def showUniPerson(p: UniPerson): String =
    p match {
      case Student(m) => "student nr " + m
      case Professor(f) => "professor on " + f
    }

  def test = {
    print(showUniPerson(Student(123)))
    print(showUniPerson(Professor("programming languages")))
  }
}


object AE {
  // Abstract Syntax Tree
  trait Exp
  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp

  // Example
  val onePlusEight = Add(Num(1), Add(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) =>
        eval(lhs) + eval(rhs)
    }
}

```