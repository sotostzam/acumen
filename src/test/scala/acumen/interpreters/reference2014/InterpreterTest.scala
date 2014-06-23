package acumen
package interpreters
package reference2014

import java.io.File
import java.io.InputStreamReader
import scala.math._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

import Errors._
import util.Filters._
import util.Names._
import util.Canonical._
import acumen.testutil.TestUtil.{
  assertEqualTrace
}

class InterpreterTest extends InterpreterTestBase with ShouldMatchers {

  override def suiteName = "Reference 2014 InterpreterTest"

  def semantics = SemanticsImpl.Ref2014

  testExamples(Examples2014, 
               {f => // The following models need closer investigation
                     // to make sure they still have the correct output.
                     // Once this is done the reference outputs in
                     // src/test/resources/acumen/data/examples-2014-res
                     // should be updated.
                     f.endsWith("/Quantization - Linear.acm") ||
                     f.endsWith("/01_Converting_Accelerations.acm") ||
                     f.startsWith("examples/XXX_internal/0_Demos") || 
                     // The ping-pong models need to be fixed.
                     f.startsWith("examples/XXX_internal/test/ping-pong")})
  testShouldRun
  
  def getError(file:String) : Option[PositionalAcumenError] = {
    try { run(file) ; None }
    catch { case e:PositionalAcumenError => Some(e) }
  }

  def testForError(file:String, err: PositionalAcumenError, pos: String) = {
    val res = getError(file) 
    res should be (Some(err))
    res.get.pos.toString should equal (pos)
  }

  test("Error1") {
    val err = ClassNotDefined(cmain)
    getError("data/ShouldCrash/Error1.acm") should be (Some(err))
  }
  test("Error2") {
    testForError("data/ShouldCrash/Error2.acm", VariableNotDeclared(name("y")), "3.12")
  }
  test("Error3") {
    testForError("data/ShouldCrash/Error3.acm", VariableNotDeclared(name("x")), "2.27")
  }
  test("Error4") {
    testForError("data/ShouldCrash/Error4.acm", UnknownOperator("f"), "3.45")
  }
  test("Error5") {
    testForError("data/ShouldCrash/Error5.acm", NotAnObject(VLit(GInt(1))), "3.24")
  }
  test("Error6") {
    testForError("data/ShouldCrash/Error6.acm", NotAnObject(VLit(GInt(1))), "2.38")
  }
  test("Error7") {
    val err = evaluating {run("data/ShouldCrash/Error7.acm")} should produce [AccessDenied[CId]]
    err.pos.toString should be ("8.3")
  }
  test("Error8") {
    val err = evaluating {run("data/ShouldCrash/Error8.acm")} should produce [AccessDenied[CId]]
    err.pos.toString should be ("27.15")
  }
  test("Error9") {
    val err = evaluating {run("data/ShouldCrash/Error9.acm")} should produce [NotAChildOf[CId]]
    err.pos.toString should be ("33.12")
  }
  test("Error10") {
    testForError("data/ShouldCrash/Error10.acm", ClassNotDefined(ClassName("B")), "15.25")
  }
  test("Error11") {
    val err = ClassDefinedTwice(ClassName("A"))
    getError("data/ShouldCrash/Error11.acm") should be (Some(err))
    // No line number
  }
  test("ACUMEN-348") {
    testForError("data/ShouldCrash/ACUMEN-348.acm", DuplicateDiscreteAssingment(Name("period",0)), "14.5")
  }


  /* tests that match theoretical values against the interpreter's values */
  type VarHistory = Stream[Tuple2[Double,Double]] 

  def oneVar(id:CId, x:String, h: Interpreter.History) : VarHistory = 
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
    (semantics.applyRequiredPasses(Parser.run(Parser.prog, p)), 
     Parser.run(Parser.store, st))

  ignore("Gravity1d") {
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
             resultType = @Discrete,
             nextChild = 0 }

      #0.2 { className = Ball,
             parent = #0,
             x = 0.0,
             x' = 0.0,
             x'' = 0.0,
             nextChild = 0 }
      """

    val (prog, store) = parse(progTxt, storeTxt)

    val h = semantics.interpreter.loop(prog, store)
    val xs = oneVar(CId(2), "x", onlyAfterContinuous(h))

    def expected(t:Double) = - (9.8 / 2.0) * t * t 
    assert(matches(xs, expected))
  }

  ignore("Gravity2d") {
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
             resultType = @Discrete,
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

    val h = onlyAfterContinuous(semantics.interpreter.loop(prog, store))
    val xs = oneVar(CId(3), "x", h)
    val ys = oneVar(CId(3), "y", h)

    def expectedy(t:Double) = - (9.8 / 2.0) * t * t + 2.0 * t
    def expectedx(t:Double) = t
    assert(matches(xs, expectedx))
    assert(matches(ys, expectedy))
  }
  
  test("continuous assignments are independent") {
    val head = "class Main(simulator)\n  private x := 0; t := 0; t' := 1 end "
    val cond = "if t < simulator.timeStep x = 1 else x = -1 end;"
    val time = "t' = 1;"
    val tail = "end"
    val timeFirst = Parser.run(Parser.prog, head ++ time ++ cond ++ tail)
    val condFirst = Parser.run(Parser.prog, head ++ cond ++ time ++ tail)
    assertEqualTrace(timeFirst, condFirst, semantics)
  }
  
}
