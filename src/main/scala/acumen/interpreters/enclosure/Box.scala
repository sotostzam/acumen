package acumen.interpreters.enclosure

import Types._

object Box {

  /** An empty box. */
  def empty: Box = Box()

  /** Build a box out of name-interval pairs. */
  def apply(domains: (VarName, Interval)*): Box = Map(domains: _*).asInstanceOf[Box]

  /** Translate the domain so that each interval is of the form [0,_]. */
  def normalize(box: Box) = box.mapValues { i => val (lo, _) = i.bounds; i - lo }

  /** The corners of the box. */
  def corners(box: Box) = cornersHelper(box, Seq(Box.empty))
  private def cornersHelper(dom: Map[VarName,Interval], corners: Seq[Map[VarName,Interval]]): Seq[Map[VarName,Interval]] =
    dom.keys.toList match {
      case Nil => corners
      case k :: ks => cornersHelper(dom - k, corners.flatMap(corner => {
        val (lo, hi) = dom(k).bounds
        Seq(corner + (k -> lo), corner + (k -> hi))
      }))
    }
  
  implicit def toBox(m:Map[VarName,Interval]):Box = m.asInstanceOf[Box]
  
}