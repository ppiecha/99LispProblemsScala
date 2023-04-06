/*
* P21 (*) Insert an element at a given position into a list.
Example:
* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)
* */

def insertAt(item: Any, list: List[Any], at: Int): List[Any] = {
  def iter: (Any, List[Any], Int) => List[Any] = {
    case (_, Nil, _) => Nil
    case (item, x :: xs, index) =>
      if (index == 1)
        item :: x :: iter(item, xs, index - 1)
      else
        x :: iter(item, xs, index - 1)
  }
  iter(item, list, at)
}

insertAt(99, List(1, 2, 3, 4), 2)