package acumen.interpreters.imperative.parallel

// FIXME: Figure out why this is failing or remove

//import org.scalacheck.Properties
//import scala.util.parsing.input.StreamReader
//import scala.collection.immutable.SortedMap
//
//object Regression extends Properties("Regression") {
//
//  import acumen.util.System._
//  import acumen.{ CId, CObject, CValue, Desugarer, Name, Parser }
//
//  val FILE_SUFFIX_RESULT = ".result"
//
//  // FIXME: fail with error message when no files are loaded
//  property("consistency") = {
//    val models = readFiles("src/test/resources/acumen/interpreters/parallel/models/", FILE_SUFFIX_MODEL)
//    val results = readFiles("src/test/resources/acumen/interpreters/parallel/expected/", FILE_SUFFIX_RESULT)
//
//    def toSortedCStore(cs: Map[CId, CObject]): SortedMap[CId, CObject] = {
//      def toSortedCObject(co: CObject): SortedMap[Name, CValue] = {
//        import Name._
//        SortedMap(co.toList.sortWith {
//          case ((n1, _), (n2, _)) => n1 < n2
//        }: _*)
//      }
//      SortedMap(cs.mapValues(toSortedCObject).toList.sortWith {
//        case ((k1, _), (k2, _)) => k1 <= k2
//      }: _*)
//    }
//
//    val i = Interpreter(2)
//    val resultKeySet = results.keySet
//    models.forall {
//      case (name, model) =>
//        try {
//          val ast = Parser.run(Parser.prog, model)
//          val des = Desugarer.run(ast)
//          val computed = toSortedCStore(i.run(des).ctrace.last).toString
//          results.get(name) match {
//            case None =>
//              println(name + ".result is missing for comparison with result:\n" + computed)
//              false
//            case Some(expected) =>
//              val success = computed == expected
//              if (!success) throw (new Error(name))
//              success
//          }
//        } catch {
//          case err =>
//            val name = err.getMessage
//            println(name + ".acm INCONSISTENT")
//            false
//        }
//    }
//  }
//
//}
