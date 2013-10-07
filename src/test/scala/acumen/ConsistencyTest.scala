package acumen

import org.scalacheck.Properties
import scala.util.parsing.input.StreamReader
import scala.collection.immutable.SortedMap

object ConsistencyTest extends Properties("parallel.ConsistencyTest") {

  import util.System._

  property("reference-parallel-consistency") = consistencyTest(interpreters.reference.Interpreter)
  
  property("parallel-parallel-consistency (determinism)") = consistencyTest(interpreters.imperative.ParallelInterpreter(2))
  
  def consistencyTest(base: CStoreInterpreter) = {
    def failWithError(msg: String): Boolean = { System.err.println(msg); false }
    def equal(expected: CStore, observed: CStore): Boolean = {
      val observedKeys = observed.keySet
      val expectedKeys = expected.keySet
      val commonKeys = observedKeys intersect expectedKeys
      val onlyObservedKeys = observedKeys -- commonKeys
      val onlyExpectedKeys = expectedKeys -- commonKeys
      if (onlyObservedKeys.nonEmpty || onlyExpectedKeys.nonEmpty)
        failWithError("Incompatible key sets: " + onlyObservedKeys.toList.sort(_ <= _) + "\n!=\n" + onlyExpectedKeys.toList.sort(_ <= _))
      else observed.keys.exists(id => {
        val obsObj = observed(id)
        val expObj = expected(id)
        if (obsObj.size != expObj.size)
          failWithError("CObjects correspoinding ton CId " + id + " differ in length.")
        else {
          val allsame = (expObj zip obsObj).forall {
            case ((expName, expValue), (obsName, obsValue)) =>
              if (expName != obsName)
                failWithError("Expected " + id + "." + obsName.x + " to have name " + expName + " but was " + obsName)
              else if (obsValue != obsValue)
                failWithError("Expected " + id + "." + expName.x + " to have value " + expValue + " but was " + obsValue)
              else true
          }
          allsame
        }
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
      System.err.println("No models loaded!")
      false
    }
    else models.forall {
      case (name, model) =>
        val ast = Parser.run(Parser.prog, model)
        val des = Desugarer.run(ast)
        val expected = (base.run(des): CStoreRes).ctrace.last
        val computed = par.run(des).ctrace.last
        if (!equal(expected, computed)) {
          System.err.println("expected: " + expected)
          System.err.println("computed: " + computed)
          System.err.println(name + ".acm INCONSISTENT")
          false
        }
        else {
          print("+")
          true
        }
    }
  }

}
