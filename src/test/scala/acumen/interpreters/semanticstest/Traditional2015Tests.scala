package acumen.interpreters.semanticstest

import acumen._
import acumen.Errors._
import acumen.testutil.TestUtil.assertEqualTrace

trait Traditional2015Tests extends Traditional2014Tests {

  override def examples : Examples = Examples2015

}
