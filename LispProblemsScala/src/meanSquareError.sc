import scala.util.chaining.scalaUtilChainingOps

object Sol {
  def solution(a: Array[Int], b: Array[Int]): Double =
    (a zip b).map{ case (x, y) =>  math.pow(y - x, 2)}.pipe(arr => arr.sum/arr.length)

Sol.solution(Array(1, 2, 3), Array(4, 5, 6))
Sol.solution(Array(10, 20, 10, 2), Array(10, 25, 5, -2))
Sol.solution(Array(-1, 0), Array(0, -1))