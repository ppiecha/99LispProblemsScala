/*
* Implement the function unique_in_order which takes as argument a sequence and returns a list of items
* without any elements with the same value next to each other and preserving the original order of elements.

For example:

uniqueInOrder("AAAABBBCCDAABBB")   == List('A', 'B', 'C', 'D', 'A', 'B')
uniqueInOrder("ABBCcAD")           == List('A', 'B', 'C', 'c', 'A', 'D')
uniqueInOrder(List(1, 2, 2, 3, 3)) == List(1, 2, 3)
* */

def op[T]: (T, List[T]) => List[T] = {
  case (x, Nil) => x :: Nil
  case (x, y :: ys) => if (x == y) y :: ys else x :: y :: ys
}

op(3, List(1, 2))
op(3, List())

def uniqueInOrder[T](xs: Iterable[T]): Seq[T] =
  xs.foldRight(List[T]()){
    case (x, Nil) => x :: Nil
    case (x, y :: ys) => if (x == y) y :: ys else x :: y :: ys
  }

uniqueInOrder[Int](List())
uniqueInOrder[Int](List(1))
uniqueInOrder[Int](List(1, 1))
uniqueInOrder[Char]("AAAABBBCCDAABBB")
uniqueInOrder[Char]("ABBCcAD")
uniqueInOrder[Int](List(1, 2, 2, 3, 3))

def uniqueInOrder2[T](xs: Iterable[T]): Seq[T] =
  if (xs.isEmpty) Nil else xs.head +: uniqueInOrder2(xs.dropWhile(_ == xs.head))