object format {
  def sumOfIntervals(intervals: List[(Int, Int)]): Int = {
    intervals.foldLeft(Array(intervals.head)) {
      case (array, pair@(left, right)) =>
        val index = array.indexWhere { case (x, y) => Range.inclusive(x, y).intersect(Range.inclusive(left, right)).nonEmpty }
        if (index >= 0) array.updated(index, (left.min(array(index)._1), right.max(array(index)._2))) else array.appended(pair)
    }.map { case (x, y) => y - x }.sum
  }
}
