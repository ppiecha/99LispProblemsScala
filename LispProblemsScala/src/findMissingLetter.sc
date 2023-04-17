/*
* Find the missing letter
Write a method that takes an array of consecutive (increasing) letters as input and that returns the missing letter in the array.

You will always get an valid array. And it will be always exactly one letter be missing. The length of the array will always be at least 2.
The array will always contain letters in only one case.

Example:

['a','b','c','d','f'] -> 'e'
['O','Q','R','S'] -> 'P'
(Use the English alphabet with 26 letters!)
* */

object Kata {
  def findMissingLetter(chars: Array[Char]): Char =
    chars.map(_.toInt).foldLeft((None: Option[Int], None: Option[Int])){
      case ((None, _), x) => (Some(x), None)
      case ((Some(previous), missing), current) =>
        if (current - previous != 1)
          (Some(current), Some(current - 1))
        else
          (Some(current), missing)
    }._2.get.toChar
  def findMissingLetter2(chars: Array[Char]): Char =
    chars.map(_.toInt).sliding(2).collectFirst{case Array(x, y) if y - x != 1 => x + 1}.get.toChar
}

Kata.findMissingLetter2(Array('a','b','c','d','f'))
Kata.findMissingLetter2(Array('O','Q','R','S'))