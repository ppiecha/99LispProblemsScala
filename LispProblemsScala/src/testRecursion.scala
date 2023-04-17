import scala.annotation.tailrec

object testRecursion extends App {
//  @tailrec
  def factorial(n: Int): BigInt = if (n <= 1) 1 else n * factorial(n - 1)

  println(factorial(4))
}
