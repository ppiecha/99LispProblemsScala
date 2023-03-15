def sort[T](list: List[T], cmp: (T, T) => Boolean): List[T] = {
  def merge(l1: List[T], l2: List[T]): List[T] = (l1, l2) match {
    case (Nil, l2) => l2
    case (l1, Nil) => l1
    case (x :: xs, y :: ys) => if (cmp(x, y)) x :: merge(xs, y :: ys) else y :: merge(x :: xs, ys)
  }

  list match {
    case Nil => Nil
    case List(x) => List(x)
    case l =>
      val (p1, p2) = l.splitAt(l.length / 2)
      merge(sort(p1, cmp), sort(p2, cmp))
  }
}

assert(sort[Int](List(1, 3, 2), (x, y) => x < y) == List(1, 2, 3))
val sortInt = sort[Int](_, (x, y) => x < y)
assert(sortInt(List(9, 8, 7, 6, 2, 3, 4, 4, 2, 7, 5, 4, 1, 1, 2)) ==
  List(1, 1, 2, 2, 2, 3, 4, 4, 4, 5, 6, 7, 7, 8, 9))
