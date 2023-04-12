/*
* Write a function that accepts an array of 10 integers (between 0 and 9), that returns a string of those numbers in the form of a phone number.

Example
Kata.createPhoneNumber(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)) # => returns "(123) 456-7890"
The returned format must be correct in order to complete this challenge.

Don't forget the space after the closing parentheses!
* */

object Kata {
  def createPhoneNumber(numbers: Seq[Int]): String =
    s"(${numbers.mkString.slice(0, 3)}) ${numbers.mkString.slice(3, 6)}-${numbers.mkString.slice(6, 10)}"
}

  Kata.createPhoneNumber(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 0))