package acumen
package util

object Transform {
  
  /** Apply all program transformations to the input Prog */
  def transform(p: Prog): Prog = {
    lazy val diffed     = SD.run(p)
    lazy val desugared  = Desugarer.run(diffed)
    lazy val dva_out    = DVA.run(desugared)
    lazy val bta_out    = BTA.run(dva_out)
    lazy val spec_out   = Specializer.run(bta_out)
    lazy val nodiff_out = AD.run(spec_out)
    nodiff_out
  }
  
}