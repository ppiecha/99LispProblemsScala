object FindTheOddInt {
  def findOdd(xs: Seq[Int]): Int = {
    val dict = scala.collection.mutable.Map[Int, Int]().withDefaultValue(0)
    for (x <- xs) {
      dict(x) += 1
    }
    dict.flatMap(kv => if (kv._2 % 2 == 1) List(kv._1) else List()).head
  }
}

FindTheOddInt.findOdd(List(20,1,-1,2,-2,3,3,5,5,1,2,4,20,4,-1,-2,5))