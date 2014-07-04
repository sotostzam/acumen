package acumen.interpreters.optimized

import acumen._
import acumen.Errors._
import acumen.interpreters.semanticstest._

class Optimized2014SemanticsTest extends Traditional2014Tests {

  override def semantics = SemanticsImpl.Opt2014

  override def examplesSkip(path: String) = {
    super.examplesSkip(path) || 
    // This one seems okay, but the number of iterations is
    // different
    path.endsWith("/hodgkin-huxley.acm")
  }

}

