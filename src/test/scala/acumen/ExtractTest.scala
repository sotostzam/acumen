package acumen

import java.io.File
import java.io.FilenameFilter
import scala.util.matching.Regex
import org.scalacheck._
import Gen._
import Shrink._
import Arbitrary.arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop._
import acumen.Pretty.pprint
import acumen.util.System.{
  readFiles, FILE_SUFFIX_MODEL
}
import testutil.ProgGenerator
import testutil.TransformationTestUtil.{
  preservesContinousSemanticsOf
}

object ExtractTest extends Properties("Extract") {

  val MODEL_PATH_TEST         = List("examples/XXX_internal/one-class",
                                     "examples/XXX_internal/esaes",
                                     "examples/XXX_internal/ha-extract")
  val MODEL_PATH_ENCLOSURE    = "examples/9_Experimental/01_Enclosures/"
  val MODEL_PATHS_SKIP        = List("03_Loops")
  
  val progGenerator = 
    new ProgGenerator( maxConditionalsPerScope     = 2
                     , maxSimulationTime           = 10.0
                     , maxClassHierarchyDepth      = 0
                     , maxClassHierarchyLayerWidth = 2
                     , minContinuousVarsPerClass   = 2
                     , maxContinuousVarsPerClass   = 4
                     )
  
  import progGenerator.arbProg
    
  /**
   * The transformation from Acumen to a hybrid automaton core language 
   * introduces (necessarily?) 
   * additional mode variables and the transformation is thus not strictly semantics preserving. 
   * However, the transformation should preserve the semantics with respect to the value of 
   * continuous variables, which this property checks.
   */
  property("semantics preserving on continuous state (example models)") =
    existingModels.map{ case (name, prog) => preservesContinousSemanticsOf(new extract.Extract(_).res, prog, Some(name)) }.foldRight(true)(_&&_) // Make sure that all models are run
    
  property("semantics preserving on continous state (random models)") =
    forAll { (p: Prog) => preservesContinousSemanticsOf(new extract.Extract(_).res, p, None) }

  /** Load models compatible with the transformation from the examples directory. */
  //FIXME Update to include multi-object models
  def existingModels(): Iterable[(String, Prog)] =
    (MODEL_PATH_TEST.flatMap{readFiles(_, FILE_SUFFIX_MODEL)} ++
     new File(MODEL_PATH_ENCLOSURE).list(new FilenameFilter ()
       { def accept(f: File, n: String) = new File(f,n).isDirectory() && !MODEL_PATHS_SKIP.contains(n) })
       .flatMap(dir => readFiles(MODEL_PATH_ENCLOSURE + dir, FILE_SUFFIX_MODEL)))
    .map { case (namePrefix, prog:String) => (namePrefix, Parser.run(Parser.prog, prog)) }
  
} 
