/*
* P14 (*) Duplicate the elements of a list.
Example:
* (dupli '(a b c c d))
(A A B B C C C C D D)
* */

def dupli(list: List[Any]): List[Any] = {
  list.flatMap(x => List(x, x))
}

dupli(List(1, 2, 3, 3, 4))

/*
* P15 (**) Replicate the elements of a list a given number of times.
Example:
* (repli '(a b c) 3)
(A A A B B B C C C)
* */

def repli(list: List[Any], count: Int): List[Any] = {
  list.flatMap(x => List.fill(count)(x))
}

repli(List(1, 2, 3), 3)