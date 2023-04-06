/*
* P19 (**) Rotate a list N places to the left.
Examples:
* (rotate '(a b c d e f g h) 3)
(D E F G H A B C)
* */

type T = List[Any]

def rotate(list: T, num: Int): T = {
    def divide: (T, (T, T), Int) => (T, T) = {
    case (Nil, result, _) => result
    case (x :: xs, (left, right), num) =>
      if (num >= 1)
        divide(xs, (left, x :: right), num - 1)
      else
        divide(xs, (x :: left, right), num - 1)
  }
  val (left, right) = divide(list, (Nil, Nil), num)
  left.reverse ::: right.reverse
}

rotate(List(1, 2, 3, 4, 5, 6, 7, 8), 3)