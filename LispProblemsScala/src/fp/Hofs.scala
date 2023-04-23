package fp

object Hofs extends App {
  def toCurry(f: (Int, Int) => Int): Int => Int => Int =
//    x => f(x, _)
    x => y => f(x, y)
  def fromCurry(f: Int => Int => Int): (Int, Int) => Int =
    (x, y) => f(x)(y)
  def compose[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))
  def andThen[A, B, C](f: A => B, g: B => C): A => C = x => g(f(x))
}
