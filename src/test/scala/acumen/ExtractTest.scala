package acumen

import java.io.File
import java.io.FilenameFilter

import scala.Array.canBuildFrom
import scala.Array.fallbackCanBuildFrom

import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

import acumen.util.System.FILE_SUFFIX_MODEL
import acumen.util.System.readFiles
import testutil.ProgGenerator
import acumen.testutil.TransformationTestUtil.preservesContinuousReferenceSemanticsOf
import acumen.testutil.TransformationTestUtil.preservesContinuousEnclosureSemanticsOf

object ExtractTest extends Properties("Extract") {

  val MODEL_PATH_TEST         = List("examples/XXX_internal/one-class",
                                     "examples/XXX_internal/esaes",
                                     "examples/XXX_internal/ha-extract")
  val MODEL_PATH_ENCLOSURE    = "examples/9_Experimental/01_Enclosures/"
  val MODEL_PATHS_SKIP        = List("03_Loops")
  val MODELS_SKIP             = List("XXX_09_Sine_self_as_Second_Derivative")
  
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
  property("reference semantics preserving on continuous state (example models)") =
    existingModels.map{ case (name, prog) => preservesContinuousReferenceSemanticsOf(new extract.Extract(_).res, prog, Some(name)) }.foldRight(true)(_&&_) // Make sure that all models are run
    
  property("enclosure semantics preserving on continuous state (enclosure example models)") =
    enclosureModels.map{ case (name, prog) => preservesContinuousEnclosureSemanticsOf(new extract.Extract(_).res, prog, Some(name)) }.foldRight(true)(_&&_) // Make sure that all models are run

  property("reference semantics preserving on continous state (random models)") =
    forAll { (p: Prog) => preservesContinuousReferenceSemanticsOf(new extract.Extract(_).res, p, None) }
    
  /** Load models compatible with the transformation from the examples directory. */
  //FIXME Update to include multi-object models
  def existingModels(): Iterable[(String, Prog)] =
    (MODEL_PATH_TEST.flatMap{readFiles(_, FILE_SUFFIX_MODEL)})
    .map(parseProg) ++ 
    enclosureModels

  /** Models restricted to the (Hybrid Automaton) syntax understood by acumen.interpreters.enclosure.Extract. */
  lazy val enclosureModels: Iterable[(String, Prog)]  = new File(MODEL_PATH_ENCLOSURE).list(
      new FilenameFilter() { 
        def accept(f: File, n: String) = new File(f, n).isDirectory() && !MODEL_PATHS_SKIP.contains(n) })
    .flatMap(dir => readFiles(MODEL_PATH_ENCLOSURE + dir, FILE_SUFFIX_MODEL))
    .filter { case (namePrefix, _) => !MODELS_SKIP.contains(namePrefix) }
    .map(parseProg)

  type ProgText = String
  type ModelName = String
    
  def parseProg(p: (ModelName, ProgText)): (ModelName, Prog)  = 
    p match { case (namePrefix, prog) => (namePrefix, Parser.run(Parser.prog, prog)) }
    
} 
