package acumen.interpreters.enclosure

import org.scalacheck.Properties
import scala.util.parsing.input.StreamReader
import acumen.interpreters.enclosure.ivp.PicardSolver
import acumen.ODETransformMode
import acumen.TopLevel

object Regression extends Properties("Regression") {

  import TestingContext._
  import acumen._
  import util.System._

  val FILE_SUFFIX_RESULT = ".result"

  property("consistency") = {
    val models = readFiles("src/test/resources/acumen/interpreters/enclosure/models/", FILE_SUFFIX_MODEL)
    val results = readFiles("src/test/resources/acumen/interpreters/enclosure/expected/", FILE_SUFFIX_RESULT)
    if (models.size == 0) {
      println("No models found!") 
      false
    } else {
      val resultKeySet = results.keySet
      models.forall {
        case (name, model) =>
          try {
            val ast = Parser.run(Parser.prog, model)
            val des = Desugarer(odeTransformMode = Local).run(ast)
            val enclosures = Interpreter.run(des).res
            results.get(name) match {
              case None =>
                println(name + ".result is missing for comparison with result:\n" + enclosures)
                false
              case Some(result) =>
                val success = enclosures.toString == result
                if (!success) {
                  println("expected: " + result)
                  println("obtained: " + enclosures.toString)
                  throw (new Error(name))
                }
                success
            }
          } catch {
            case err =>
              val name = err.getMessage
              err.printStackTrace
              println(name + ".acm INCONSISTENT")
              false
          }
      }
    } 
  }

}
