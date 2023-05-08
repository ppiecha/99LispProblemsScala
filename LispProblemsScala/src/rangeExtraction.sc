object Kata {

  def solution(xs: List[Int]): String = {

    def format_buff(list: List[Int]): List[String] =
      if (list.size > 2) List(list.head.toString + "-" + list.last.toString)
      else list.map(_.toString)

    def loop(list: List[Int], buff: List[Int], res: List[String]): List[String] = (list, buff, res) match {
      case (List(), _, _) => List()
      case (List(x), buff, res) =>
        //println(x, buff, res)
        if (buff.nonEmpty)
          if (x - buff.head == 1) res ::: format_buff((x :: buff).reverse)
          else (res ::: format_buff(buff.reverse)) :+ x.toString
        else res :+ x.toString
      case (x :: xs, buff, res) =>
        //println(x, buff, res)
        if (buff.isEmpty || (x - buff.head == 1)) loop(xs, x :: buff, res)
        else loop(xs, List(x), res ::: format_buff(buff.reverse))
    }
    loop(xs, List(), List()).mkString(",")
  }
}

Kata.solution(List(-10, -9, -8, -6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20))
Kata.solution(List())
Kata.solution(List(1))
Kata.solution(List(1, 2))
Kata.solution(List(1, 2, 4))