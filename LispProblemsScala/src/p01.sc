/*
P01 (*) Find the last box of a list.
Example:
* (my-last '(a b c d))
(D)
*/

def last(list: List[Any]): Option[Any] = list match {
  case Nil => None
  case List(x) => Some(x)
  case _::xs => last(xs)
}

assert(last(List(1, 2, 3)).contains(3))
assert(last(List()).isEmpty)
