import scala.annotation.tailrec

/*
* P24 (*) Lotto: Draw N different random numbers from the set 1..M.
The selected numbers shall be returned in a list.
Example:
* (lotto-select 6 49)
(23 1 17 33 21 37)
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

def lottoSelect(n : Int, m: Int): List[Int] = rndSelect((1 to m).toList, n)

lottoSelect(6, 49)