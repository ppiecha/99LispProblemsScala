/*
* P28 (**) Sorting a list of lists according to length of sublists
a) We suppose that a list contains elements that are lists themselves.
* The objective is to sort the elements of this list according to their length.
* E.g. short lists first, longer lists later, or vice versa.

Example:
* (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))

b) Again, we suppose that a list contains elements that are lists themselves.
* But this time the objective is to sort the elements of this list according to their length frequency;
* i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first,
* others with a more frequent length come later.

Example:
* (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
((I J K L) (O) (A B C) (F G H) (D E) (D E) (M N))

Note that in the above example, the first two lists in the result have length 4 and 1, both lengths appear just once.
* The third and forth list have length 3 which appears twice (there are two list of this length).
* And finally, the last three lists have length 2. This is the most frequent length.
* */

def sortByLength[A](list: List[List[A]]): List[List[A]] = list.sortBy(_.length)

sortByLength(List(List(1), List(2, 3), List(4, 5), List(6)))

def sortByFreq[A](list: List[List[A]]): List[List[A]] =
  list.groupBy(_.length).toList.sortBy(_._2.length).flatMap(_._2)

sortByFreq(List(List(1, 2, 3, 4), List(2, 3, 4), List(4, 5, 5), List(6)))