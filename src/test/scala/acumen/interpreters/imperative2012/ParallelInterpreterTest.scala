package acumen
package interpreters
package imperative2012

import Pretty._

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

import Common._

import java.io.FileInputStream
import java.io.InputStreamReader

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite


class ParallelInterpreterTest extends FunSuite with ShouldMatchers {
  override def suiteName = "Imperative/Parallel 2012 Unit Tests"
  def semantics : SemanticsImpl.CStore = SemanticsImpl.Parallel2012()
     
  test("StoreConversions1") {
    import ParallelInterpreter._
    val txt = """
      #0 { className = Main, parent = none, 
					 nextChild = 0, seed1 = 0, seed2 = 1 }
    """
    val cst = ApproximateRationals.run(Parser.run(Parser.store, txt), TraditionalInterpreterType)
    val st = fromCStore(cst,CId())
    cst should be (repr(st))
  }
  
  test("StoreConversions2") {
    import ParallelInterpreter._
    val txt = """
      #0   { className = Main, parent = none, 
						 nextChild = 3, seed1 = 0, seed2 = 1 }
      #0.1 { className = A, parent = #0, o = #0.2, 
						 nextChild = 0, seed1 = 2, seed2 = 3 }
      #0.2 { className = A, parent = #0, o = #0.1, 
						 nextChild = 0, seed1 = 4, seed2 = 5 }
      """
    val cst = ApproximateRationals.run(Parser.run(Parser.store, txt), TraditionalInterpreterType)
    val st = fromCStore(cst, CId())
    cst should be (repr(st))
  }

  test("StoreConversions3") {
    import ParallelInterpreter._
    val txt = """
#0.1 {
  parent = #0.2,
  time = 7.596000000000177,
  className = Simulator,
  resultType = @Discrete,
  endTime = 10.0,
  timeStep = 0.0030,
  nextChild = 0,
	seed1 = 0, 
	seed2 = 1
}
#0.2 {
  parent = none,
  mode = "Persist",
  className = Main,
  simulation = #0.1,
  nextChild = 0,
	seed1 = 2, 
	seed2 = 3
}
#0.4 {
  x' = 14.112000000000132,
  parent = #0.2,
  x'' = 9.8,
  x = 89.72769879999998,
  mode = "Fly",
  className = Ball,
  nextChild = 0,
	seed1 = 4, 
	seed2 = 5
}
#0.5 {
  x' = 14.112000000000132,
  parent = #0.2,
  x'' = 9.8,
  x = 89.72769879999998,
  mode = "Fly",
  className = Ball,
  nextChild = 0,
	seed1 = 6, 
	seed2 = 7
}
"""
    val cst = ApproximateRationals.run(Parser.run(Parser.store, txt), TraditionalInterpreterType)
    val st = fromCStore(cst, CId(2))
    cst should be (repr(st))
  }
}
