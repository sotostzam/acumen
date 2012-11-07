package acumen
package tests

import Errors._
import util.Filters._
import util.Names._
import util.Canonical._
import interpreters.reference.Interpreter

import scala.math._

import java.io.FileInputStream
import java.io.InputStreamReader

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

class InterpreterTest extends Suite with ShouldMatchers {

  def run(filename:String) = {
    val in  = 
      new InputStreamReader(this.getClass.getResourceAsStream("/acumen/"+filename))
  	val ast = Parser.run(Parser.prog, in)
    val des = Desugarer.run(ast)
    Interpreter.run(des)
  }
  
  def test_bouncing_ball_1d = {
    (for (_ <- run("examples/bouncing_ball_1d.acm")) ()) should be ()
  }
  def test_bouncing_ball_2d = {
    for (_ <- run("examples/bouncing_ball_2d.acm")) () should be ()
  }
  def test_bouncing_ball_3d = {
    for (_ <- run("examples/bouncing_ball_3d.acm")) () should be ()
  }
  def test_breaking_ball_1d = {
    for (_ <- run("examples/breaking_ball_1d.acm")) () should be ()
  }
  def test_breaking_ball_2d = {
    for (_ <- run("examples/breaking_ball_2d.acm")) () should be ()
  }
  def test_breaking_ball_3d = {
    for (_ <- run("examples/breaking_ball_3d.acm")) () should be ()
  }
  def test_iccps_mass_pendulum = {
    for (_ <- run("examples/iccps_mass_pendulum.acm")) () should be ()
  }
  def test_iccps_pendulum = {
    for (_ <- run("examples/iccps_pendulum.acm")) () should be ()
  }
  def test_nbodies = {
    for (_ <- run("examples/nbodies.acm")) () should be ()
  }
  def test_random = {
    for (_ <- run("examples/random.acm")) () should be ()
  }
  
  def testShouldRun1 = {
    for (_ <- run("data/ShouldRun/shouldRun1.acm")) () should be ()
  }
  def testShouldRun2 = {
    for (_ <- run("data/ShouldRun/shouldRun2.acm")) () should be ()
  }
  def testShouldRun3 = {
    for (_ <- run("data/ShouldRun/shouldRun3.acm")) () should be ()
  }
  def testShouldRun4 = {
    for (_ <- run("data/ShouldRun/shouldRun4.acm")) () should be ()
  }
  def testShouldRun5 = {
    for (_ <- run("data/ShouldRun/shouldRun5.acm")) () should be ()
  }
  def testShouldRun6 = {
    for (_ <- run("data/ShouldRun/shouldRun6.acm")) () should be ()
  }

  def getError(file:String) : Option[AcumenError] = {
    try { for (_ <- run(file)) (); None }
    catch { case e:AcumenError => Some(e) }
  }

  def testError1 = {
    val err = ClassNotDefined(cmain)
    getError("data/ShouldCrash/Error1.acm") should be (Some(err))
  }
  def testError2 = {
    val err = VariableNotDeclared(name("y"))
    getError("data/ShouldCrash/Error2.acm") should be (Some(err))
  }
  def testError3 = {
    val err = VariableNotDeclared(name("x"))
    getError("data/ShouldCrash/Error3.acm") should be (Some(err))
  }
  def testError4 = {
    val err = UnknownOperator("f")
    getError("data/ShouldCrash/Error4.acm") should be (Some(err))
  }
  def testError5 = {
    val err = NotAnObject(VLit(GInt(1)))
    getError("data/ShouldCrash/Error5.acm") should be (Some(err))
  }
  def testError6 = {
    val err = NotAnObject(VLit(GInt(1)))
    getError("data/ShouldCrash/Error6.acm") should be (Some(err))
  }
  def testError7 = {
    val err = AccessDenied(CId(), CId(1), Nil)
    getError("data/ShouldCrash/Error7.acm") should be (Some(err))
  }
  def testError8 = {
    val err = AccessDenied(CId(0,0,1), CId(1), List(CId(1,1),CId(0,1)))
    getError("data/ShouldCrash/Error8.acm") should be (Some(err))
  }
  def testError9 = {
    val err = NotAChildOf(CId(0,0,1), CId(0,1))
    getError("data/ShouldCrash/Error9.acm") should be (Some(err))
  }
  def testError10 = {
    val err = ClassNotDefined(ClassName("B"))
    getError("data/ShouldCrash/Error10.acm") should be (Some(err))
  }
  def testError11 = {
    val err = ClassDefinedTwice(ClassName("A"))
    getError("data/ShouldCrash/Error11.acm") should be (Some(err))
  }

  /* tests that match theoretical values against the interpreter's values */
  type VarHistory = Stream[Tuple2[Double,Double]] 

  def oneVar(id:CId, x:String, h:Interpreter.History) : VarHistory = 
    h map (st => 
      (getTime(st), getObjectField(id, Name(x, 0), st)) match {
        case (t, VLit(GDouble(d))) => (t,d)
        case _  => throw VariableNotDeclared(Name(x,0))
      }
    )

  def matches(s:VarHistory, f:Double => Double) : Boolean = 
    if (s.isEmpty) true
    else {
      val (t,x) = s.head
      if (abs(f(t) - x) > 1e-2) false
      else matches(s.tail, f)
    }

  def parse(p:String, st:String) = 
    (Desugarer.run(Parser.run(Parser.prog, p)), 
     Parser.run(Parser.store, st))

  def testGravity1d = {
    val progTxt =
      """
      class Simulator(time, timeStep, endTime, stepType) end
      class Ball(x, x', x'') 
        self.x'' = -9.8
      end
      class Main(mode, simulation) end
      """

    val storeTxt = 
      """
      #0 { className = Main,
           parent = none, 
           nextChild = 0 }

      #0.1 { className = Simulator,
             parent = #0, 
             time = 0.0, 
             timeStep = 0.001, 
             endTime = 2.0, 
             stepType = @Discrete,
             nextChild = 0 }

      #0.2 { className = Ball,
             parent = #0,
             x = 0.0,
             x' = 0.0,
             x'' = 0.0,
             nextChild = 0 }
      """

    val (prog, store) = parse(progTxt, storeTxt)

    val h = Interpreter.loop(prog, store)
    val xs = oneVar(CId(2), "x", onlyAfterContinuous(h))

    def expected(t:Double) = - (9.8 / 2.0) * t * t 
    assert(matches(xs, expected))
  }

  def testGravity2d = {
    val progTxt =
      """
      class Simulator(time, timeStep, endTime, stepType) end
      class Ball(x, x', x'', y,y',y'') 
        self.y'' = -9.8;
        self.x'' = 0
      end
      class Main(mode, simulation) end
      """

    val storeTxt = 
      """
      #0 { className = Main,
           parent = none,
           nextChild = 0 }

      #0.2 { className = Simulator,
             parent = #0, 
             time = 0.0, 
             timeStep = 0.001, 
             endTime = 2.0, 
             stepType = @Discrete,
             nextChild = 0 }

      #0.3 { className = Ball,
             parent = #0,
             x   = 0.0, 
             x'  = 1.0, 
             x'' = 0.0,
             y   = 0.0,
             y'  = 2.0,
             y'' = 0.0,
             nextChild = 0 }
      """

    val (prog, store) = parse(progTxt, storeTxt)

    val h = onlyAfterContinuous(Interpreter.loop(prog, store))
    val xs = oneVar(CId(3), "x", h)
    val ys = oneVar(CId(3), "y", h)

    def expectedy(t:Double) = - (9.8 / 2.0) * t * t + 2.0 * t
    def expectedx(t:Double) = t
    assert(matches(xs, expectedx))
    assert(matches(ys, expectedy))
  }

}
