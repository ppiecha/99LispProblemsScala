import scala.annotation.tailrec

/*
* P17 (*) Split a list into two parts; the length of the first part is given.
Do not use any predefined functions.

Example:
* (split '(a b c d e f g h i k) 3)
( (A B C) (D E F G H I K))
* */

def split(list: List[Any], at: Int): (List[Any], List[Any]) = {
  @tailrec
  def iterate(list: List[Any], index: Int, result: (List[Any], List[Any])): (List[Any], List[Any]) =
    (list, index, result) match {
    case (Nil, _, res) => res
    case (x :: xs, i, (left, right)) =>
      if (i >= 1)
        iterate(xs, i - 1, (left ::: List(x) , right))
      else
        iterate(xs, i - 1, (left , right ::: List(x)))
  }
  iterate(list, at, (Nil, Nil))
}

def split2(list: List[Any], at: Int): (List[Any], List[Any]) = {
  def iterate: (List[Any], Int, (List[Any], List[Any])) => (List[Any], List[Any]) = {
      case (Nil, _, res) => res
      case (x :: xs, i, (left, right)) =>
        if (i >= 1)
          iterate(xs, i - 1, (left ::: List(x) , right))
        else
          iterate(xs, i - 1, (left , right ::: List(x)))
    }
  iterate(list, at, (Nil, Nil))
}
type T = List[Any]
def split3(list: T, at: Int): (T, T) = {
  def iterate: (T, Int, (T, T)) => (T, T) = {
    case (Nil, _, res) => res
    case (x :: xs, i, (left, right)) =>
      if (i >= 1)
        iterate(xs, i - 1, ((left ::: List(x) , right)))
      else
        iterate(xs, i - 1, (left , right ::: List(x)))
  }
  iterate(list, at, (List(), List()))
}

split(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 3)
split2(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 3)
split3(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 3)

