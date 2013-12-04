package acumen
package util

object Transform {
  
  /** Apply all program transformations to the input Prog */
  def transform(p: Prog): Prog = {
    lazy val diffed     = SD.run(p)
    lazy val desugared  = Desugarer().run(diffed)
    desugared
  }
  
}
