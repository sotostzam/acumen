package acumen
package util

import Canonical._
import Pretty._
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
      prettyRealVector(rv))

  def printStore(title: String, st: CStore) =
    println((if (title != "") title + ": " else "") +
      pprint(Pretty prettyStore st))

  def printRealMatrix(title: String, m: RealMatrix, indexToName: Int => (CId, Name)) =
    println((if (title != "") title + ": " else "") +
      prettyRealMatrix(m, indexToName(_).toString))

  def printRealMatrix(title: String, m: RealMatrix) =
    println((if (title != "") title + ": " else "") +
      prettyRealMatrix(m, _.toString))

  def printStateOf(names: List[Name], st: CStore) =
    names.foreach { n =>
      st.foreach { case (_, co) =>
        co.foreach{ case (n1, v) =>
            if (n1 == n) println(pprint(n) + ": " + pprint(v))
        }
      }
    }
      
}