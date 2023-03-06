/*
P02 (*) Find the last but one box of a list.
Example:
  * (my-but-last '(a b c d))
(C D)
*/

def lastButOne(list: List[Any]): List[Any] = list match {
  case Nil => Nil
  case List(x) => List(x)
  case List(x, y) => List(x, y)
  case _::xs => lastButOne(xs)
}

assert(lastButOne(List(1, 2, 3)) == List(2, 3))
assert(lastButOne(Nil).isEmpty)
assert(lastButOne(List(1)) == List(1))
assert(lastButOne(List(1, 2)) == List(1, 2))