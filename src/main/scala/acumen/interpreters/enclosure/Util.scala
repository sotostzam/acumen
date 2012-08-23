package acumen.interpreters.enclosure

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

}