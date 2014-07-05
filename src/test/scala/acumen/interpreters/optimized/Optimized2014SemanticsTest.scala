package acumen.interpreters.optimized

import acumen._
import acumen.Errors._
import acumen.interpreters.semanticstest._

class Optimized2014SemanticsTest extends Traditional2014Tests {

  override def semantics = SemanticsImpl.Opt2014

}

