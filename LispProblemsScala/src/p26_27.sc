/*
* P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Example:
* (combination 3 '(a b c d e f))
((A B C) (A B D) (A B E) ... )
* */

def allSubsets[A](set: Set[A]): Set[Set[A]] =
  if (set.isEmpty) Set(Set())
  else {
    for {
      heads <- Set(Set(), Set(set.head))
      tail <- allSubsets(set.tail)
    } yield heads ++ tail
  }

def allSubLists[A](list: List[A]): List[List[A]] = list match {
  case Nil => List(Nil)
  case h :: t =>
    for {
      head <- List(Nil, List(h))
      tail <- allSubLists(t)
    } yield head ::: tail
}

allSubLists(List('a', 'b', 'c'))
allSubLists(List('b', 'c'))

def combination[A](count: Int, list: List[A]): List[List[A]] =
  (count, list) match {
    case (_, Nil) => List(Nil)
    case (0, _) => List(Nil)
    case (count, h :: t) =>
      for {
        head <- List(Nil, List(h))
        tail <- allSubLists(t)
        //tail <- combination(count - 1, t) if !tail.contains(h)
        if (head ::: tail).length == count
      } yield head ::: tail
}

combination(1, List('b', 'c')).foreach(println)
combination(2, List('a', 'b', 'c')).foreach(println)
combination(3, List('a', 'b', 'c', 'd')).foreach(println)
combination(2, List('a', 'b', 'c', 'd')).foreach(println)

/*
* P27 (**) Group the elements of a set into disjoint subsets.
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
* Write a function that generates all the possibilities and returns them in a list.

Example:
* (group3 '(aldo beat carla david evi flip gary hugo ida))
( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
... )

b) Generalize the above function in a way that
* we can specify a list of group sizes and the function will return a list of groups.

Example:
* (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
... )

Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
* */

def group3[A](list: List[A]): List[List[List[A]]] =
  for {
    t2 <- combination(2, list)
    t3 <- combination(3, list diff t2)
    t4 <- combination(4, (list diff t2) diff t3)
  } yield List(t2, t3, t4)

group3(List("aldo", "beat", "carla", "david", "evi", "flip", "gary", "hugo", "ida")).foreach(println)

def group3[A](list: List[A], g1: Int, g2: Int, g3: Int): List[List[List[A]]] =
  for {
    t1 <- combination(g1, list)
    t2 <- combination(g2, list diff t1)
    t3 <- combination(g3, (list diff t1) diff t2)
  } yield List(t1, t2, t3)

group3(List("aldo", "beat", "carla", "david", "evi", "flip", "gary", "hugo", "ida"), 2, 2, 5).foreach(println)