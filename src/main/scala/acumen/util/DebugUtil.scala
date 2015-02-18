package acumen
package util

import Canonical._

object DebugUtil {
  
  /** Update p so that the initially section of the Main ClassDef matches 
   *  the values in st. Can e.g. be used to re-start the simulation in a 
   *  specific state. 
   *  Note: Only works on models consisting of a single model definition
   *        and only updates the instance variables of the Main object. */
  def asProgram(st: CStore, base: Prog): Prog =
    Prog(base.defs.filter(_.name != cdevice).map{ d =>
      if (d.name == cmain) d.copy(priv = d.priv.map{ f =>
        f.copy(rhs = (f.rhs: @unchecked) match {
          case ExprRhs(_) =>
            val VLit(v) = st(mainId(st))(f.x)
            ExprRhs(Lit(v))
        })
      }) else d
    })

}