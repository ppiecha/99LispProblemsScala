/*
* P20 (*) Remove the K'th element from a list.
Example:
* (remove-at '(a b c d) 2)
(A C D)
* */

def removeAt(list: List[Any], k: Int): List[Any] = {
  def iter: (List[Any], Int) => List[Any] = {
    case (Nil, _) => Nil
    case (x :: xs, index) => if (index == 1) iter(xs, index - 1) else x :: iter(xs, index - 1)
  }
  iter(list, k)
}

removeAt(List(1, 2, 3, 4), 2)