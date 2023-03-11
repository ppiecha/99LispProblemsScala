/*
P09 (**) Pack consecutive duplicates of list elements into sublists.
  If a list contains repeated elements they should be placed in separate sublists.

  Example:
  * (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))
*/

def pack(list: List[Any]): List[List[Any]] = list match {
  case Nil => Nil
  case list => list.foldLeft(Nil: List[List[Any]])((acc, item) => (acc, item) match {
    case (Nil, item) => List(List(item))
    case (acc, item) if acc.last.last == item => acc.init ::: List(item :: acc.last)
    case _ => acc ::: List(List(item))
  }
  )
}

assert(pack(List(1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5)) ==
  List(List(1, 1, 1, 1), List(2), List(3, 3), List(1, 1), List(4), List(5, 5, 5, 5)))
