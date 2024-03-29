import scala.annotation.tailrec

/*
* P23 (**) Extract a given number of randomly selected elements from a list.
The selected items shall be returned in a list.
Example:
* (rnd-select '(a b c d e f g h) 3)
(E D A)
* Hint: Use the built-in random number generator and the result of problem P20.
* */

def rndSelect[A](list: List[A], count: Int): List[A] = {
  @tailrec
  def loop(seq: Seq[A], count: Int, acc: Seq[A]): Seq[A] = (seq, count) match {
    case (Nil, _) => acc
    case (_, 0) => acc
    case (_, n) =>
      val rand = new scala.util.Random().nextInt(seq.length)
      loop(seq.filter(_ != seq(rand)), n - 1, seq(rand) +: acc)
  }
  loop(list.toSeq, count, Seq()).toList
}

rndSelect(List(1, 2, 3, 4, 5, 6, 7), 3)