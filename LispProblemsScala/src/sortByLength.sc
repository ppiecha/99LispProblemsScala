object Kata {

  def sortByLength(arr: Array[String]): Array[String] =
    arr.sortBy(_.size)
}

Kata.sortByLength(Array("beg", "life", "i", "to"))