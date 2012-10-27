package acumen.interpreters.enclosure

import java.io.FileWriter

object Util {

  /**
   * Given two maps, creates a new map which has as keys the union of the keys
   * of the input maps, and as values pairs of elements from the two input maps.
   * The "default" value is used when one of the maps does not contain a key
   * which occurs in the other map.
   */
  def zipDefault[K, V](left: Map[K, V], right: Map[K, V], default: V) =
    (left.keySet union right.keySet).map { key =>
      key ->
        (left.getOrElse(key, default), right.getOrElse(key, default))
    }.toMap

  /** TODO add description */
  def newFile(path: String) = {
    val fw = new FileWriter(path, false)
    try {
      fw.write("")
    } finally fw.close
  }

  /** TODO add description */
  def appendFile(path: String, str: String) = {
    val fw = new FileWriter(path, true)
    try {
      fw.write(str)
    } finally fw.close
  }

  def fixedPoint[A](f: A => A)(x: A)(implicit rnd: Rounding): A = {
    val y = f(x)
    if (x == y) x
    else fixedPoint(f)(y)
  }

}
