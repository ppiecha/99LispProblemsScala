/*
* If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Finish the solution so that it returns the sum of all the multiples of 3 or 5 below the number passed in.
* Additionally, if the number is negative, return 0 (for languages that do have them).

Note: If the number is a multiple of both 3 and 5, only count it once.

Courtesy of projecteuler.net (Problem 1)
* */

object MultiplesOf3Or5 {
  def solution(number: Int): Long =
    if (number < 3)
      0
    else {
      (3 until number).map(_.toLong).filter(x => (x % 3) == 0 || (x % 5 == 0)).sum
    }

  def solution2(number: Int): Long =
    (0 to (number / 3).max(number / 5)).flatMap{ x => List(x * 3, x * 5) }.distinct.sum
}

MultiplesOf3Or5.solution(10)
MultiplesOf3Or5.solution(15)
MultiplesOf3Or5.solution(16)
MultiplesOf3Or5.solution(20)
MultiplesOf3Or5.solution(100000)


MultiplesOf3Or5.solution2(10)
MultiplesOf3Or5.solution2(15)
MultiplesOf3Or5.solution2(16)
MultiplesOf3Or5.solution2(20)
