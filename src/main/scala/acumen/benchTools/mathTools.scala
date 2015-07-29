/*
 * File holding all the mathematical tools used to synthesize the results
 */
package benchTool

object mathTools {
  type timeT = bench.timeT
  type LLtimeT = bench.LLtimeT
  type ResultT = bench.ResultT
  type RefineResultT = bench.RefineResultT

  /**
   * Return the average and the standard deviation of the list of time
   */
  def std_deviations(list: List[timeT]): (timeT, Double) = {
    val average = list.sum / list.size
    (average, math.sqrt(list.map(x => (x - average) * (x - average)).sum))
  }
  
  /**
   * Return the five quartiles of the list of time
   */
  def quartiles(list: List[timeT]): (timeT, timeT, timeT, timeT, timeT) = {
    val s = list.sortWith(_ < _)
    s.length match {
      case 0 | 1 => (s.sum, s.sum, s.sum, s.sum, s.sum)
      case 2     => (s.head, s.head, (s.head + s.last) / 2, s.last, s.last)
      // none of the quartiles are among the data
      case lsize if lsize % 4 == 0 =>
        (s(0),
          (s(lsize / 4) + s(lsize / 4 - 1)) / 2.0,
          (s(lsize / 2) + s(lsize / 2 - 1)) / 2.0,
          (s(3 * lsize / 4) + s(3 * lsize / 4 - 1)) / 2.0,
          s.last)
      // the median is not among the data
      case lsize if lsize % 4 == 2 =>
        (s(0),
          s((lsize - 2) / 4),
          (s(lsize / 2) + s(lsize / 2 - 1)) / 2.0,
          s((3 * lsize - 2) / 4),
          s.last)
      case lsize if lsize % 4 == 1 =>
        // both Q1 and Q3 are not among the data
        (s(0),
          1.0 / 4.0 * s((lsize - 5) / 4) + 3.0 / 4.0 * s((lsize - 1) / 4),
          s((lsize - 1) / 2),
          1.0 / 4.0 * s(3 * (lsize - 1) / 4) + 3.0 / 4.0 * s((3 * lsize + 1) / 4),
          s.last)
      case lsize =>
        // both Q1 and Q3 are not among the data
        (s(0),
          1.0 / 4.0 * s((lsize + 3) / 4) + 3.0 / 4.0 * s((lsize - 1) / 4),
          s((lsize - 1) / 2),
          1.0 / 4.0 * s((3 * lsize - 5) / 4) + 3.0 / 4.0 * s((3 * lsize - 1) / 4),
          s.last)
    }
  }

  /**
   * Computation of the minimum and the lower and upper ranges for the results
   */
  def min_and_quartile(list: List[timeT]): (timeT, timeT, timeT) = {
    val (q0, q1, q2, q3, q4) = quartiles(list)
    val n = list.size
    (q0, (q1 - q0) / n, ((q1 + q2 + q3 + q4) / 4 - q0) / n)
  }

  /**
   * Apply the quartile transformation for all elements of a resultT
   */
  def applyMinAndQuartiles(total: ResultT): RefineResultT = {
    for (tot <- total) yield tot match {
      case (mod, inter, times, a, b) =>
        (mod, inter, times.transpose.map(mathTools.min_and_quartile), a, b)
    }
  }
/**
 * Computation of the median and some range based on quartiles
 */
  def median_and_quartiles(list: List[timeT]): (timeT, Double, timeT, timeT) = {
    val (q0, q1, q2, q3, q4) = quartiles(list)
    (q2, 100.0 * (q3 - q1) / q2, q1, q3)
  }
}	
