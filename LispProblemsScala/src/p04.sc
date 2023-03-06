// P04 (*) Find the number of elements of a list.

def len(list: List[Any] = Nil): Int = list match {
  case Nil => 0
  case _::xs => 1 + len(list = xs)
}

assert(len() == 0)
assert(len(List(1, 2)) == 2)