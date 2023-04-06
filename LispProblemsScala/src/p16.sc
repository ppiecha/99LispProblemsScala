/*
* P16 (**) Drop every N'th element from a list.
Example:
* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)
* */

def drop(list: List[Any], nth: Int): List[Any] = {
  def iterate(list: List[Any], index: Int): List[Any] = (list, index) match {
    case (Nil, _) => Nil
    case (_ :: xs, 1) => iterate(xs, nth)
    case (x :: xs, i) => x :: iterate(xs, i - 1)
  }
  iterate(list, nth)
}

drop(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 3)