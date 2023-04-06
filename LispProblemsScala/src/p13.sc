/*
* P13 (**) Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them.
* As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example:
* (encode-direct '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
* */

sealed trait Element

case class Single(element: Any) extends Element
case class Multiple(num_elem: (Int, Any)) extends Element

def encode_direct(list: List[Any]): List[Element] = {
  list.foldRight(List(): List[Element])((x, acc) => acc match {
    case Nil => List(Single(x))
    case y :: ys => y match {
      case Single(element) => if (element == x) Multiple((2, x)) :: ys else Single(x) :: y :: ys
      case Multiple((num, elem)) => if (elem == x) Multiple((num + 1, elem)) :: ys else Single(x) :: y :: ys
    }
  })
}

encode_direct(List(1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5))