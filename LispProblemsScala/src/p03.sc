/*
P03 (*) Find the K'th element of a list.
  The first element in the list is number 1.
Example:
  * (element-at '(a b c d e) 3)
C
*/

def elementAt(list: List[Any], index: Int): Option[Any] = {
  def loop(list: List[Any], index: Int): Option[Any] = (list, index) match{
    case (Nil, _) => None
    case (list, index) if index > list.length => None
    case (x::_, 1) => Some(x)
    case (_::xs, index) => loop(list = xs, index = index - 1)
  }
  loop(list = list, index = index)
}

assert(elementAt(List(1, 2, 3), 2).contains(2))
assert(elementAt(List(), 1).isEmpty)
assert(elementAt(List(1), 2).isEmpty)
assert(elementAt(List(1), 1).contains(1))
