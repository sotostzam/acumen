package acumen

import util.Names._

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

class ParserTest extends Suite with ShouldMatchers {
  def testParseVar() {
    Parser.run(Parser.expr,"x") should be (Var(name("x")))
  }
}
