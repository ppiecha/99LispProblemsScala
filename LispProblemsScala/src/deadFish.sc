/*
* Write a simple parser that will parse and run Deadfish.

Deadfish has 4 commands, each 1 character long:

i increments the value (initially 0)
d decrements the value
s squares the value
o outputs the value into the return array
Invalid characters should be ignored.

Deadfish.parse("iiisdoso") => List(8, 64)
* */

object DeadFish {
  def parse(data: String): List[Int] =
    data.foldLeft(0, List[Int]()){
      case ((current, output), op) => op match {
        case 'i' => (current + 1, output)
        case 'd' => (current - 1, output)
        case 's' => (current * current, output)
        case 'o' => (current, output ::: List(current))
        case _ => (current, output)
      }
    }._2
}

DeadFish.parse("iiisdoso")