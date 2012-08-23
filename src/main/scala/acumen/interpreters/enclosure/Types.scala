package acumen.interpreters.enclosure

object Types {

  /* Type synonyms */

  /** Interval boxes with named components. */
  type Box = Map[VarName, Interval]

  object Box {

    /** An empty box. */
    def empty: Box = Box()

    /** Build a box out of name-interval pairs. */
    def apply(domains: (VarName, Interval)*): Box = Map(domains: _*)

    /** Translate the domain so that each interval is of the form [0,_]. */
    def normalize(box: Box) = box.mapValues { i => val (lo, _) = i.bounds; i - lo }

    /** The corners of the box. */
    def corners(box: Box) = cornersHelper(box, Seq(Box.empty))
    private def cornersHelper(dom: Box, corners: Seq[Box]): Seq[Box] =
      dom.keys.toList match {
        case Nil => corners
        case k :: ks => cornersHelper(dom - k, corners.flatMap(corner => {
          val (lo, hi) = dom(k).bounds
          Seq(corner + (k -> lo), corner + (k -> hi))
        }))
      }
  }

  /** Variable name */
  type VarName = String

}

object Moo extends App {
  import Types._
  implicit val r = Rounding(10)
  println(Box.empty)
}