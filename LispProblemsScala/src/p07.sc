/*
P07 (**) Flatten a nested list structure.
Example:
  * (my-flatten '(a (b (c d) e)))
(A B C D E)
*/

def flatten(list: List[_]): List[Any] = list match {
  case Nil => Nil
  case (x: List[_]) :: xs => flatten(x) ::: flatten(xs)
  case x :: xs => x :: flatten(xs)
}
assert(flatten(Nil).isEmpty)
assert(flatten(1::Nil) == List(1))
assert(flatten(1::(2::Nil)::Nil) == List(1,2))
assert(flatten(1::(2::3::Nil)::Nil) == List(1,2,3))
assert(flatten(List('a', List('b', List('c','d'), 'e'))) == List('a', 'b', 'c', 'd', 'e'))

def flatten2(list: List[Any]): List[Any] = list flatMap {
  case xs: List[_] => flatten2(xs)
  case x => List(x)
}

flatten2(List('a', List('b', List('c','d'), 'e')))