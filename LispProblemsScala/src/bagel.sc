/*
* Here's a seemingly simple challenge. We're giving you a class called bagel, exactly as it appears below. All it really does is return an int, specifically 3.

class Bagel {
  final def getValue = 3
}
The catch? For the solution, we're testing that the result is equal to 4. But as a little hint, the solution to this Kata is (almost) exactly the same as the example test cases.
* */

class Bagel {
  final def getValue = 3
}

//object Bagel {
//  override def equals(obj: Any): Boolean = obj == 4
//}

//trait Value {
//  def getValue: () => Int
//}

//implicit def get

object BagelSolver {
  def getBagel: Bagel = {
    new Bagel {
//      override def equals(n: Any): Boolean = n == 4
//      def getValue(s: String): (Int) => Boolean = _ == 4
      def `getValue.==`(n: Int): Boolean = n == 4
    }
  }

}

def value(s: String = " ==")(i: Int = 4): Boolean = s + i.toString == " == 4"

var b: Bagel = BagelSolver.getBagel
b.getValue
b.getValue.==(4)

//class BagelSpec extends FunSpec with Matchers {
//  describe("Bagel") {
//    it("test") {
//      var b: Bagel = BagelSolver.getBagel
//      b.getValue == 4 shouldBe java.lang.Boolean.TRUE
//    }
//  }
//}

