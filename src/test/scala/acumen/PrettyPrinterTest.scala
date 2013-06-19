package acumen

import Pretty._

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

class PrettyPrinterTest extends Suite with ShouldMatchers {

  def testBool1() {
    val ast : Value[_] = VLit(GBool(true))
    pprint(ast) should be ("true")
  }
  
  def testBool2() {
    val ast : Value[_] = VLit(GBool(false))
    pprint(ast) should be ("false")
  }
}
