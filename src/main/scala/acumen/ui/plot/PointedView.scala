package acumen.ui
package plot

import swing._
import swing.event._

class PointedView(tv:PlotTab) extends Label with Reactor {

 text = " "
 listenTo(tv)

 reactions += {
   case PointedAtEvent(t,name,value) =>
     text = name + " @ " + t + " = " + value
 }
}
