def flatMap(list: List[Any]): List[Any] = list match {
  case Nil => Nil
  case (x: List[_]) :: xs => flatMap(x) ::: flatMap(xs)
  case x :: xs => x :: flatMap(xs)
}

flatMap(List(List(1, 2), List(2, 3)))



