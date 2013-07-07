package acumen

import org.scalacheck.Properties
import scala.util.parsing.input.StreamReader
import scala.collection.immutable.SortedMap

object ConsistencyTest extends Properties("parallel.ConsistencyTest") {

  sealed abstract class Moo(x: Int)
  class Boo(x: Int, y: Boolean) extends Moo(x)

  import util.System._

  property("reference-parallel-consistency") = consistencyTest(interpreters.reference.Interpreter)
  
  property("parallel-parallel-consistency (determinism)") = consistencyTest(interpreters.imperative.ParallelInterpreter(2))
  
  def consistencyTest(base: CStoreInterpreter) = {
    def notEqual(left: CStore, right: CStore): Boolean = {
      val leftKeys = left.keySet
      val rightKeys = right.keySet
      val commonKeys = leftKeys intersect rightKeys
      val onlyLeftKeys = leftKeys -- commonKeys
      val onlyRightKeys = rightKeys -- commonKeys
      if (onlyLeftKeys.nonEmpty || onlyRightKeys.nonEmpty) {
        println(onlyLeftKeys.toList.sort(_ <= _) + "\n!=\n" + onlyRightKeys.toList.sort(_ <= _))
        true
      }
      else left.keys.exists(id => {
        val l = left(id)
        val r = right(id)
        if (l != r) { println(id + "" + left(id)); true } else false
      })
    }
    // These interpreters are compared by the test.
    // They are run on the models in the directory 
    // src/test/resources/acumen/interpreters/parallel/models/
    // and the last CStore of the resulting Histories 
    // are compared for structural equality.
    val par = interpreters.imperative.ParallelInterpreter(2)
    val models = readFiles("src/test/resources/acumen/interpreters/parallel/models/", FILE_SUFFIX_MODEL)
    if (models.isEmpty) {
      println("no models loaded!")
      false
    }
    else models.forall {
      case (name, model) =>
        val ast = Parser.run(Parser.prog, model)
        val des = Desugarer.run(ast)
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
