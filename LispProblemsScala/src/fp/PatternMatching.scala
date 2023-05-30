package fp

object PatternMatching extends App {
  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr

  def decode(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => s"(${decode(e1)} + ${decode(e2)})"
    case Prod(e1, e2) => s"${decode(e1)} * ${decode(e2)}"
  }

  println(decode(Sum(Number(2), Number(3))))
  println(decode(Sum(Sum(Number(2), Number(3)), Number(4))))
  println(decode(Prod(Sum(Number(2), Number(1)), Number(3))))
  //println(decode(Prod(Sum(Number(2), Number(1)), Number(3))))

  

}
