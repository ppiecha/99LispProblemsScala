/*
* P22 (*) Create a list containing all integers within a given range.
If first argument is smaller than second, produce a list in decreasing order.
Example:
* (range 4 9)
(4 5 6 7 8 9)
* */

def range(rng: (Int, Int)): List[Any] = {
  val ordered_range = if (rng._1 > rng._2) rng.swap else rng
  def gen: ((Int, Int)) => List[Any] = {
    case (_, 0) => Nil
    case (start, count) => start :: gen(start + 1, count - 1)
  }
  val res = gen((ordered_range._1, ordered_range._2 - ordered_range._1 + 1))
  if (rng._1 > rng._2) res.reverse else res
}

range((4, 9))
range((9, 4))