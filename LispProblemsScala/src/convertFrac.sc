/*
* Common denominators

You will have a list of rationals in the form

{ {numer_1, denom_1} , ... {numer_n, denom_n} }
or
[ [numer_1, denom_1] , ... [numer_n, denom_n] ]
or
[ (numer_1, denom_1) , ... (numer_n, denom_n) ]
where all numbers are positive ints. You have to produce a result in the form:

(N_1, D) ... (N_n, D)
or
[ [N_1, D] ... [N_n, D] ]
or
[ (N_1', D) , ... (N_n, D) ]
or
{{N_1, D} ... {N_n, D}}
or
"(N_1, D) ... (N_n, D)"
depending on the language (See Example tests) in which D is as small as possible and

N_1/D == numer_1/denom_1 ... N_n/D == numer_n,/denom_n.
Example:
convertFracs [(1, 2), (1, 3), (1, 4)] `shouldBe` [(6, 12), (4, 12), (3, 12)]
Note:
Due to the fact that the first translations were written long ago - more than 6 years - these first translations have only irreducible fractions.

Newer translations have some reducible fractions. To be on the safe side it is better to do a bit more work by simplifying fractions even if they don't have to be.
* */

import scala.util.chaining._

object Fracts {
  def convertFrac(lst: Array[(Long, Long)]): String = {

    def nwd(a: Long, b: Long): Long = (a, b) match {
      case (a, 0) => a
      case (a, b) => nwd(b, a % b)
    }

    def nwd_list(list: List[Long]): Long = list match {
      case l if l.length < 2 => throw new Exception("not enough arguments")
      case List(a, b) => nwd(a, b)
      case x :: xs => nwd_list(x :: nwd_list(xs) :: Nil)
    }

    def nww(list: List[Long]): Long = list match {
      case List(a) => a
      case List(a, b) => (a * b) / nwd(a, b)
      case x :: xs => nww(x :: nww(xs) :: Nil)
    }

    val d = nww(lst.toList.map(_._2))
    lst.map {
      pair => {
        val m = d / pair._2
        (pair._1 * m, pair._2 * m)
      }
    }.pipe{
      list => {
        val norm = nwd_list(list.head._2 ::list.toList.map(_._1))
        list.map(pair => (pair._1 / norm, pair._2 / norm))
      }
    }.mkString
  }
}

Fracts.convertFrac(Array((69, 130), (87, 1310), (30, 40)))
Fracts.convertFrac(Array((3, 1)))
