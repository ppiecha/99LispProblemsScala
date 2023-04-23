package fp

object FunctionTypes extends App {

  val concat: (String, String) => String = new Function2[String, String, String] {
    override def apply(v1: String, v2: String): String = v1 + v2
  }
  val concat1: (String, String) => String = { case (x, y) => x + y }
  val concat2: (String, String) => String = (x: String, y: String) =>  x + y

  def multipleTimes(n: Int): Int => Int =
    x => n * x

  def doubler = multipleTimes(2)

  println(doubler(3))
  println(multipleTimes(4)(3))

  println(concat("s1", "s2"))
  println(concat1("s1", "s2"))
  println(concat2("s1", "s2"))
}
