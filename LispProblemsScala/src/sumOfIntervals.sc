/*
* Write a function called sumIntervals/sum_intervals that accepts an array of intervals, and returns the sum of all the interval lengths. Overlapping intervals should only be counted once.

Intervals
Intervals are represented by a pair of integers in the form of an array. The first value of the interval will always be less than the second value. Interval example: [1, 5] is an interval from 1 to 5. The length of this interval is 4.

Overlapping Intervals
List containing overlapping intervals:

[
   [1, 4],
   [7, 10],
   [3, 5]
]
The sum of the lengths of these intervals is 7. Since [1, 4] and [3, 5] overlap, we can treat the interval as [1, 5], which has a length of 4.

Examples:
sumIntervals( [
   [1, 2],
   [6, 10],
   [11, 15]
] ) => 9

sumIntervals( [
   [1, 4],
   [7, 10],
   [3, 5]
] ) => 7

sumIntervals( [
   [1, 5],
   [10, 20],
   [1, 6],
   [16, 19],
   [5, 11]
] ) => 19

sumIntervals( [
   [0, 20],
   [-100000000, 10],
   [30, 40]
] ) => 100000030
* */

import scala.util.chaining._

def sumOfIntervals(intervals: List[(Int, Int)]): Int = {
  intervals.sorted.foldLeft(Array[(Int, Int)]()) {
    case (array, pair) if array.isEmpty => Array(pair)
    case (array, (x, y)) => array.last match {
      case (left, right) if x <= right => array.dropRight(1) :+ (left, right.max(y))
      case (_, right) if x > right => array :+ (x, y)
      }
    }.foldLeft(0){ case (acc, (a, b)) => acc + b - a }
}

def sumOfIntervals2(intervals: List[(Int, Int)]): Int =
  intervals.view.sorted.scanLeft((Int.MinValue, Int.MinValue)){
    case ((_, x), (b, e)) => (b max x, e max x)
  }.map{case (b, e) => e - b}.sum

sumOfIntervals(List((0, 20), (-1000000000, 10), (30, 40)))
sumOfIntervals(List((1, 2), (3, 4)))
sumOfIntervals(List((1, 3), (3, 4)))
sumOfIntervals(List((-1000000000, 1000000000)))
sumOfIntervals(List((1,4), (7,10), (3,5)))
//.tap(_.foreach(println))