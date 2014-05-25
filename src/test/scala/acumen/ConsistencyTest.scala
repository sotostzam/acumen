package acumen

import org.scalacheck.Properties
import scala.util.parsing.input.StreamReader
import scala.collection.immutable.SortedMap

import testutil.TestUtil.notEqual 

object ConsistencyTest extends Properties("parallel.ConsistencyTest") {

  import util.System._

  property("reference-parallel-consistency") = consistencyTest(interpreters.reference2013.Interpreter)
  
  property("parallel-parallel-consistency (determinism)") = consistencyTest(interpreters.imperative2012.ParallelInterpreter(2))
  
  def consistencyTest(base: CStoreInterpreter) = {
    // These interpreters are compared by the test.
    // They are run on the models in the directory 
    // src/test/resources/acumen/interpreters/parallel/models/
    // and the last CStore of the resulting Histories 
    // are compared for structural equality.
    val par = interpreters.imperative2012.ParallelInterpreter(2)
    val models = readFiles("src/test/resources/acumen/interpreters/parallel/models/", FILE_SUFFIX_MODEL)
    if (models.isEmpty) {
      println("no models loaded!")
      false
    }
    else models.forall {
      case (name, model) =>
        val ast = Parser.run(Parser.prog, model)
        val des = Desugarer().run(ast)
        val expected = (base.run(des): CStoreRes).ctrace.last
        val computed = par.run(des).ctrace.last
        if (notEqual(computed, expected)) {
          println("expected: " + expected)
          println("computed: " + computed)
          println(name + ".acm INCONSISTENT")
          false
        }
        else true
    }
  }

}
