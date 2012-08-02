package acumen
package render

import util.Filters._

object ToPython {

  def toPython2D(s:Summary2D) = {
    for ((id,(s,xs,ys)) <- s) {
      println(s)
      for (d <- xs) { print(d); print(" ") }
      println()
      for (d <- ys) { print(d); print(" ") }
      println()
    } 
  }
  
  def toPython3D(s:Summary3D) {
    for ((id,(s,xs,ys,zs)) <- s) {
      println(s)
      for (d <- xs) { print(d); print(" ") }
      println()
      for (d <- ys) { print(d); print(" ") }
      println()
      for (d <- zs) { print(d); print(" ") }
      println()
    } 
  }  

}
