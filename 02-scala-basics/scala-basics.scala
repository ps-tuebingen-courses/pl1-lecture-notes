val x = 2 + 2

val y: Int = 2 + 2

def square(n: Int): Int = n * n

square(5)

var counter = 0
counter = counter + 1

abstract class Person(val name: String) {
  def sayHello: Unit = print("hello, " + name)
}

trait ByeBye {
  def sayGoodbye: Unit = print("bye")
}

class Tillmann(name: String) extends Person(name) with ByeBye {
  override def sayHello: Unit = print("hi")
}

object Me extends Tillmann("Tillmann")

Me.sayHello

sealed trait UniPerson
case class Student(id: Int) extends UniPerson
case class Professor(subject: String) extends UniPerson

def describe(p: UniPerson): String =
  p match {
    case Student(id)      => "student nr " + id
    case Professor(field) => "professor of " + field
  }

describe(Student(123))
describe(Professor("programming languages"))

enum UniPersonEnum:
  case StudentEnum(id: Int)
  case ProfessorEnum(subject: String)

import UniPersonEnum._

def describeEnum(p: UniPersonEnum): String =
  p match {
    case StudentEnum(id)      => "student nr " + id
    case ProfessorEnum(field) => "professor of " + field
  }

describeEnum(StudentEnum(123))

object AE {
  // Abstract syntax: an expression is a number, or the addition of two expressions.
  enum Exp:
    case Num(n: Int)
    case Add(lhs: Exp, rhs: Exp)

  import Exp._

  // An example expression, standing for 1 + (5 + 3):
  val onePlusEight = Add(Num(1), Add(Num(5), Num(3)))

  // The interpreter: recurse over the structure of the expression.
  def eval(e: Exp): Int =
    e match {
      case Num(n)        => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
    }
}

AE.onePlusEight

AE.eval(AE.onePlusEight)

