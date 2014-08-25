package acumen
package util

object Names {
  def name(x:String) = Name(x, 0)
  def op(o: String, ps: Expr*) = Op(name(o), ps.toList)
}
