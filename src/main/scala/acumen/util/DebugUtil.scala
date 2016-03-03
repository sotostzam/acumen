package acumen
package util

import Canonical._
import interpreters.enclosure2015.Common._

object DebugUtil {
  
  /** Update p so that the initially section of the Main ClassDef matches 
   *  the values in st. Can e.g. be used to re-start the simulation in a 
   *  specific state. 
   *  Note: Only works on models consisting of a single model definition
   *        and only updates the instance variables of the Main object. */
  def asProgram(st: CStore, base: Prog): Prog =
    if (base.defs.size > 3) base
    else
      Prog(base.defs.filter(_.name != cdevice).map{ d =>
        if (d.name == cmain) d.copy(priv = d.priv.map{ f =>
          f.copy(rhs = (f.rhs: @unchecked) match {
            case ExprRhs(_) =>
              val VLit(v) = st(mainId(st))(f.x)
              ExprRhs(Lit(v))
          })
        }) else d
      })

  def printRealVector(title: String, rv: RealVector) =
    println((if (title != "") title + ": " else "") +
      rv.toArray.zipWithIndex.map { case (v, i) => v }.mkString(", "))

  def printStore(title: String, st: CStore) =
    println((if (title != "") title + ": " else "") +
      (Pretty pprint (Pretty prettyStore st)))

  def printRealMatrix(title: String, m: RealMatrix, indexToName: Int => (CId, Name)) {
    var row = 0
    if (title != "") println(title + ": ")
    print(s"${indexToName(row)}\t")
    m.foreachKey {
      case (r, c) =>
        m(r, c) match {
          case VLit(GConstantRealEnclosure(i)) =>
            if (r > row) { row += 1; print(s"\n${indexToName(row)}\t") }
            print(s"$i\t")
        }
    }
    print("\n")
  }

  def printRealMatrix(title: String, m: RealMatrix) {
    var row = 0
    if (title != "") println(title + ": ")
    print(s"${row}\t")
    m.foreachKey {
      case (r, c) =>
        m(r, c) match {
          case VLit(GConstantRealEnclosure(i)) =>
            if (r > row) { row += 1; print(s"\n${row}\t") }
            print(s"$i\t")
        }
    }
    print("\n")
  }

}