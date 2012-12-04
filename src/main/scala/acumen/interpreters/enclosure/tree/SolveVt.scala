package acumen.interpreters.enclosure.tree

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Interval._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.UnivariateAffineEnclosure._

trait SolveVt {

  /**
   * Solves an ODE-IVP given by a field F for a time interval T and
   * initial condition A by iteratively applying the Picard operator.
   *
   *  	x' 		=	F(x)
   * 	x(T.lo) in	A
   *
   * It does this in the following way:
   *
   * 1. Initialize by representing each interval A_i in A as a variable a_i.
   * 	So, A ~ a = (a_1,...,a_n) which is used in place of A during computation.
   * 2. Build a function enclosure over T x A by iterating the Picard operator obtained from F.
   * 3. Collapse the obtained enclosure onto an enclosure over T.
   *
   */
  def solveVt(
    F: Field, // field
    T: Interval, // domain of t
    A: Box, // (A1,...,An), initial condition
    delta: Double, // padding 
    m: Int, // extra iterations after inclusion of iterates
    n: Int, // maximum number of iterations before inclusion of iterates
    degree: Int // number of pieces to split each initial condition interval 
    )(implicit rnd: Rounding): UnivariateAffineEnclosure = {

    val timeName = A.keys.fold("_")(_ + _)
    val a = initialConditionsAsFunctions(timeName, A, T)
    // First approximation of the solution
    val Y0 = a.plusMinus(delta) // [(t,a1,...,an) -> (a1+[-d,d],...,an+[-d,d])]
    val Q = picard(timeName, a, F)_
    // Iterate the Picard operator until we obtain an enclosure for the solution over the entire domain.
    // This occurs when the next approximation is contained within the current one.
    var current = Y0
    var next = Q(current)
    var i = 0
    while (!current.contains(next) && !iterationLimitReached(i, n)(T)) {
      current = next
      next = Q(current)
      i += 1
    }
    improveApproximation(current, Q, m) // Apply the Picard operator an additional m times
    UnivariateAffineEnclosure(convertToSolutionOnlyOfTime(current, timeName, T))
  }

  /**
   * Iteration may not terminate (e.g. when F is not Leipzig) and is stopped
   * after n attempts.
   */
  private def iterationLimitReached(i: Int, n: Int)(T: Interval)(implicit rnd: Rounding) =
    if (i < n) false else sys.error("solveVt: terminated at " + T + " after " + n + " Picard iterations")

  /**
   * Obtain a solution approximation that is a function of 't' only.
   * Naively, this could be thought of as replacing each occurrence of a_i
   * in the solution with its corresponding A_i.
   */
  private def convertToSolutionOnlyOfTime(approx: AffineEnclosure, timeName: VarName, T: Interval)(implicit rnd: Rounding) = {
    val onNornaizedDomain = approx.collapse((approx.domain.keys.toList - timeName): _*)
    AffineEnclosure(
      onNornaizedDomain.domain.mapValues(_ => T), // assuming only variable is timeName
      onNornaizedDomain.components.mapValues {
        case ase =>
          AffineScalarEnclosure(ase.domain.mapValues(_ => T), ase.constant, ase.coefficients)
      })
  }

  /**
   * The Picard operator
   */
  private def picard(timeName: VarName, a: AffineEnclosure, F: Field)(X: AffineEnclosure)(implicit rnd: Rounding): AffineEnclosure = {
    a + (F(X).primitive(timeName))
  }

  /**
   * Represent each interval A_i in A as a variable a_i, corresponding to an
   * identity function \x. x
   */
  private def initialConditionsAsFunctions(timeName: VarName, A: Box, T: Interval)(implicit rnd: Rounding) = {
    val domain = A + (timeName -> 0 /\ T.width) // NOTE: translated t to [0,T.hi-T.lo]
    AffineEnclosure(domain, A.keys.toSeq: _*) // [(t,a1,...,an) -> (a1,...,an)]
  }

  /**
   * Improve an approximation by applying Q (a Picard operator with a given
   * field and initial condition) m times.
   */
  private def improveApproximation(approximation: AffineEnclosure, Q: AffineEnclosure => AffineEnclosure, m: Int): AffineEnclosure = {
    var next = approximation
    var current = next
    for (j <- 0 until m) {
      current = next
      next = Q(current)
    }
    next
  }

}