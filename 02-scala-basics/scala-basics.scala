val x = 2 + 2;

// val für Konstanten
// def für Methoden
// var für Variablen

abstract class Person(val name: String) {
  // String name
  // val name: String

  def sayHello = {
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


enum UniPersonEnum:
  case StudentEnum(val matrikelnummer: Int)
  case ProfessorEnum(val fachgebiet: String)

object PatternMatchingEnum {
  import UniPersonEnum._

  def showUniPerson(p: UniPersonEnum): String =
    p match {
      case StudentEnum(m) => "student nr " + m
      case ProfessorEnum(f) => "professor on " + f
    }

  def test = {
    print(showUniPerson(StudentEnum(123)))
    print(showUniPerson(ProfessorEnum("programming languages")))
  }
}


object AE {
  // Abstract Syntax Tree
  enum Exp:
    case Num(n: Int)
    case Add(lhs: Exp, rhs: Exp)

  import Exp._

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

