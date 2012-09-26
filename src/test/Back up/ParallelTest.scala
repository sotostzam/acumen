package acumen
package tests

import Pretty._

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

import java.io.FileInputStream
import java.io.InputStreamReader

class ParallelTest extends Suite with ShouldMatchers {

  def testStoreConversions1 = {
    import interpreters.parallel.Interpreter._
    val txt = """
      #0 { className = Main, parent = none, 
					 nextChild = 0, seed1 = 0, seed2 = 1 }
    """
    val cst = Parser.run(Parser.store, txt)
    val st = fromCStore(cst,CId())
    cst should be (repr(st))
  }
  
  def testStoreConversions2 = {
    import interpreters.parallel.Interpreter._
    val txt = """
      #0   { className = Main, parent = none, 
						 nextChild = 3, seed1 = 0, seed2 = 1 }
      #0.1 { className = A, parent = #0, o = #0.2, 
						 nextChild = 0, seed1 = 2, seed2 = 3 }
      #0.2 { className = A, parent = #0, o = #0.1, 
						 nextChild = 0, seed1 = 4, seed2 = 5 }
      """
    val cst = Parser.run(Parser.store, txt)
    val st = fromCStore(cst,CId())
    cst should be (repr(st))
  }

  def testStoreConversions3 = {
    import interpreters.parallel.Interpreter._
    val txt = """
#0.1 {
  parent = #0.2,
  time = 7.596000000000177,
  className = Simulator,
  stepType = @Discrete,
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
    val cst = Parser.run(Parser.store, txt)
    val st = fromCStore(cst,CId(2))
    cst should be (repr(st))
  }

  def eqstreams(s1:Seq[CStore], s2:Seq[CStore]) : Boolean= {
    var t1 = s1
    var t2 = s2 
    var break = false
    while (t1.nonEmpty && t2.nonEmpty && !break) {
      val h1 = t1.head
      val h2 = t2.head
      t1 = t1.tail
      t2 = t2.tail
      if (h1 != h2) {
        println("*" * 30)
        println(pprint(prettyStore(h1)))
        println("!=" * 10)
        println(pprint(prettyStore(h2)))
        println("*" * 30)
        break = true
      }
    }
    if (break) false
    else t1.isEmpty && t2.isEmpty
  }

  def run(filename:String) = {
    val RI = interpreters.reference.Interpreter
    val PIO = interpreters.parallel.Interpreter
    val in  = 
      new InputStreamReader(this.getClass.getResourceAsStream("/acumen/"+filename))
  	val ast = Parser.run(Parser.prog, in)
    val des = Desugarer.run(ast)
    val trace1 = RI.run(des)
    PIO.withInterpreter(2) { PI => 
      eqstreams(trace1, PI.run(des) map PI.repr) 
    }
  }
  def test_bouncing_ball_1d = {
    assert(run("examples/bouncing_ball_1d.acm"))
  }
  def test_bouncing_ball_2d = {
    assert(run("examples/bouncing_ball_2d.acm"))
  }
  def test_bouncing_ball_3d = {
    assert(run("examples/bouncing_ball_2d.acm"))
  }
  def test_breaking_ball_1d = {
    assert(run("examples/breaking_ball_1d.acm"))
  }
  def test_breaking_ball_2d = {
    assert(run("examples/breaking_ball_2d.acm"))
  }
  def test_breaking_ball_3d = {
    assert(run("examples/breaking_ball_3d.acm"))
  }
  def test_iccps_mass_pendulum = {
    assert(run("examples/iccps_mass_pendulum.acm"))
  }
  def test_iccps_pendulum = {
    assert(run("examples/iccps_pendulum.acm"))
  }
  def test_nbodies = {
    assert(run("examples/nbodies.acm"))
  }
  def test_random = {
    assert(run("examples/random.acm"))
  }
  /*
  def test_bh = {
    assert(run("examples/bh.acm"))
  }
  */
  def test_shouldRun1 = {
    assert(run("data/ShouldRun/shouldRun1.acm"))
  }
  def test_shouldRun2 = {
    assert(run("data/ShouldRun/shouldRun2.acm"))
  }
  def test_shouldRun3 = {
    assert(run("data/ShouldRun/shouldRun3.acm"))
  }
  def test_shouldRun4 = {
    assert(run("data/ShouldRun/shouldRun4.acm"))
  }
  def test_shouldRun5 = {
    assert(run("data/ShouldRun/shouldRun5.acm"))
  }
  def test_shouldRun6 = {
    assert(run("data/ShouldRun/shouldRun6.acm"))
  }
}
