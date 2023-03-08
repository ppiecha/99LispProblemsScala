/*
P08 (**) Eliminate consecutive duplicates of list elements.
  If a list contains repeated elements they should be replaced with a single copy of the element.
  The order of the elements should not be changed.

  Example:
  * (compress '(a a a a b c c a a d e e e e))
(A B C A D E)
*/

def compress(list: List[Any]): List[Any] = list match {
  case Nil => Nil
  case list => list.foldLeft(Nil: List[Any])((acc, item) =>
    acc ::: (if (acc.nonEmpty && acc.last == item) Nil else List(item)))
}

assert(compress(List(1, 2, 2, 3)) == List(1, 2, 3))
assert(compress(List(1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5)) == List(1, 2, 3, 1, 4, 5))
