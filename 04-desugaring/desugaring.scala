object AE {
  // Abstract Syntax Tree
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
  import Exp._

  // Example
  val ex = Add(Num(1), Add(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) =>
        eval(lhs) + eval(rhs)
    }
}

object MAE {
  // Abstract Syntax Tree
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
    case Mult(lhs: Exp, rhs: Exp) extends Exp
  import Exp._

  // Example
  val ex = Add(Num(1), Mult(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}

object SMAE {
  // Abstract Syntax Tree
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
    case Mult(lhs: Exp, rhs: Exp) extends Exp
    case Sub(lhs: Exp, rhs: Exp) extends Exp
  import Exp._

  // Example
  val ex = Sub(Num(1), Mult(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
      case Sub(lhs, rhs) =>  eval(lhs) - eval(rhs)
    }
}

def desugarSMAE2MAE(e: SMAE.Exp): MAE.Exp = e match {
  case SMAE.Exp.Num(n) => MAE.Exp.Num(n)
  case SMAE.Exp.Add(lhs, rhs) => MAE.Exp.Add(desugarSMAE2MAE(lhs), desugarSMAE2MAE(rhs))
  case SMAE.Exp.Mult(lhs, rhs) => MAE.Exp.Mult(desugarSMAE2MAE(lhs), desugarSMAE2MAE(rhs))
  case SMAE.Exp.Sub(lhs, rhs) =>
    MAE.Exp.Add(desugarSMAE2MAE(lhs),
                 MAE.Exp.Mult(MAE.Exp.Num(-1), desugarSMAE2MAE(rhs)))
}

val res = MAE.eval(desugarSMAE2MAE(SMAE.ex))

object SMAE2 {
  // Abstract Syntax Tree
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
    case Mult(lhs: Exp, rhs: Exp) extends Exp
  object Exp:
    def sub(e1: Exp, e2: Exp): Exp =
      Add(e1, Mult(Num(-1), e2))
  import Exp._

  // Compared to SMAE, we only have to change upper case Sub by lower case sub
  // when constructing examples.
  val ex = sub(Num(1), Mult(Num(5), Num(3)))

  // Interpreter - no case for sub needed
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}

object USMAE {
  // Abstract Syntax Tree
  enum Exp:
    case Num(n: Int) extends Exp
    case Add(lhs: Exp, rhs: Exp) extends Exp
    case Mult(lhs: Exp, rhs: Exp) extends Exp
  object Exp:
    def sub(e1: Exp, e2: Exp): Exp =
      Add(e1, Mult(Num(-1), e2))
    def unaryminus(e: Exp) = sub(Num(0), e)
  import Exp._

  // Compared to SMAE, we only have to change upper case Sub by lower case sub
  // when constructing examples.
  val ex = sub(unaryminus(Num(1)), Mult(Num(5), Num(3)))

  // Interpreter - no case for sub needed
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}

