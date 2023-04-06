/*
* P18 (**) Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the
* original list (both limits included). Start counting the elements with 1.

Example:
* (slice '(a b c d e f g h i k) 3 7)
(C D E F G)
* */
type T = List[Any]
def slice(list: T, pair: (Int, Int)): T = {
  list.drop(pair._1 - 1).take(pair._2 - pair._1 + 1)
}

slice(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), (3, 7))