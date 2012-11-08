package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Types._
import UnivariateAffineEnclosure._

trait SolveVtE {

  // TODO add description
  def detectNextEvent(
    H: HybridSystem,
    T: Interval,
    q: Mode,
    Y: UnivariateAffineEnclosure)(implicit rnd: Rounding): Outcome = {
    /**
     * This is likely where we get the issues with enclosures exploding from. The
     * events that are deemed possible are determined by evaluating the guard over
     * the range of the enclosure, rather than directly on the enclosure which is
     * a major source of imprecision!
     */
    val events = H.events.filter(e => e.sigma == q && H.guards(e)(Y.range) != Set(false))
    //    println("\ndetectNextEvent: E' = " + events) // PRINTME

    val res =
      if (events.isEmpty) {
        //        println("detectNextEvent: " + MaybeOneOf(events) + "\n") // PRINTME
        MaybeOneOf(events)
      } else {
        if (H.domains(q)(Y(Y.domain.high)) == Set(false)) {
          //          println("detectNextEvent: " + H.domains(q)) // PRINTME
          //          println("detectNextEvent: " + "Y(" + T.high + ") = " + Y(Y.domain.high)) // PRINTME
          //          println("detectNextEvent: " + CertainlyOneOf(events)) // PRINTME
          CertainlyOneOf(events)
        } else {
          //          println("detectNextEvent: " + MaybeOneOf(events)) // PRINTME
          MaybeOneOf(events)
        }
      }
    //    println("detected: " + res)
    res
  }

  // TODO add description
  def solveVtE(
    H: HybridSystem,
    T: Interval,
    S: UncertainState,
    delta: Double,
    m: Int,
    n: Int,
    K: Int,
    output: String,
    log: String => Unit)(implicit rnd: Rounding): Option[(Set[UncertainState], Seq[UnivariateAffineEnclosure])] = {
    var res = EventTree.initialTree(T, H, S, delta, m, n, output)
    var tmp = res.addLayer
    while (res.size < K && tmp != res) {
      res = tmp
      tmp = tmp.addLayer
    }
    if (res != tmp)
      None
    else {
      //      println("\nsolveVtE: " + res.endTimeStates.map(_.initialCondition)) // PRINTME
      //      println("\nsolveVtE: " + res.prunedEnclosures)
      Some((res.endTimeStates, res.prunedEnclosures))
      //      Some((res.endTimeStates, res.unprunedEnclosures))
    }
  }

}
