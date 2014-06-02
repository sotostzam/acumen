package acumen
package interpreters
package imperative2012

import Pretty._

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

import Common._

import java.io.FileInputStream
import java.io.InputStreamReader

class ImperativeInterpreterTest extends ParallelInterpreterTest {
  override def semantics = SemanticsImpl.Imperative2012
}
