import scala.annotation.tailrec

/*
* P25 (*) Generate a random permutation of the elements of a list.
Example:
* (rnd-permu '(a b c d e f))
(B A D C E F)
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

def rndPermu[A](list: List[A]): List[A] = rndSelect(list, list.length)

rndPermu(List('a', 'b', 'c', 'd', 'e', 'f'))