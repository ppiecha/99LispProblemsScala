package fp

object Curry extends App {
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  val add7_1: Int => Int = y => y + 7
  val add7_2: Int => Int = simpleAddFunction(7, _)
  val add7_3: Int => Int = simpleAddMethod(7, _)
  val add7_4: Int => Int = curriedAddMethod(7)(_)
  val add7_5: Int => Int = curriedAddMethod(7)
  val add7_6: Int => Int = curriedAddMethod(7) _
  val add7_7: Int => Int = (x: Int) => simpleAddFunction(7, x)
  val add7_8: Int => Int = (x: Int) => simpleAddMethod(7, x)
  val add7_9: Int => Int = (x: Int) => curriedAddMethod(7)(x)
  val add7_10: Int => Int = simpleAddFunction.curried(7)

  println(add7_6(1))

  def byName(n: => Int) = n + 1
  println(((x: Int) => x * 2)(2))

  def sideEffectCondition: Boolean = {
    println("boo")
    true
  }

  def simpleCondition: Boolean = false

  println(if (simpleCondition && sideEffectCondition) "yes" else "no")

}
