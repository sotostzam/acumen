
package repeat

import org.junit._
import Assert._

class Tests {
  @Test def testEval1() {
    assertEquals(repeat.Interp.eval(Const(7)), 7)
    assertEquals(repeat.Interp.eval(Plus(Const(1),Const(5))), 6)
    assertEquals(repeat.Interp.eval(Minus(Const(1),Const(5))), -4)
  }

  @Test def testEval2() {
    assertEquals(repeat.Interp.eval(Repeat("x",1,3,"acc",5,Plus(Var("x"),Var("acc")))), 11)
  }

  @Test def testEval3() {
    assertEquals(repeat.Interp.eval(Repeat("i",1,1000,"a",0,Plus(Var("i"),Var("a")))), 500500)
  }

}