package acumen

import java.io.File
import java.io.FilenameFilter
import scala.Array.canBuildFrom
import scala.Array.fallbackCanBuildFrom
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties
import acumen.testutil.TransformationTestUtil.countModes
import acumen.testutil.TransformationTestUtil.preservesContinuousEnclosureSemanticsOf
import acumen.testutil.TransformationTestUtil.preservesContinuousReferenceSemanticsOf
import acumen.testutil.TransformationTestUtil.printErrUnless
import acumen.util.System.FILE_SUFFIX_MODEL
import acumen.util.System.readFiles
import testutil.ProgGenerator
import acumen.testutil.TransformationTestUtil.userModeVariables
import acumen.passes.ExtractHA

trait ExtractTest {

  val MODEL_PATH_TEST         = List("examples/05_Language_Research/13_One-class",
                                     "examples/05_Language_Research/06_Esaes",
                                     "examples/05_Language_Research/09_Ha-extract")
  val MODEL_PATH_ENCLOSURE    = "examples/04_Experimental/01_Enclosures/"
  val MODEL_PATHS_SKIP        = List("03_Loops")
  val MODELS_SKIP             = List("XXX_09_Sine_self_as_Second_Derivative")
  
  val progGenerator = 
    new ProgGenerator( maxConditionalsPerScope     = 2
                     , maxSimulationTime           = 10.0
                     , maxClassHierarchyDepth      = 0
                     , maxClassHierarchyLayerWidth = 1
                     , minContinuousVarsPerClass   = 2
                     , maxContinuousVarsPerClass   = 4
                     )

  /** First desugar p and then return the result of applying transform. */
  //def desugarAndTransform(p: Prog, transform: Prog => Prog) = transform(Desugarer() run p)
  
  /** Load models compatible with the transformation from the examples directory. */
  //FIXME Update to include multi-object models
  def existingModels(): Iterable[(String, Prog)] =
    (MODEL_PATH_TEST.flatMap{readFiles(_, FILE_SUFFIX_MODEL)})
    .map(parseProg(TraditionalInterpreterType)(_)) ++ 
    enclosureModels

  /** Models restricted to the (Hybrid Automaton) syntax understood by acumen.interpreters.enclosure.Extract. */
  lazy val enclosureModels: Iterable[(String, Prog)]  = new File(MODEL_PATH_ENCLOSURE).list(
      new FilenameFilter() { 
        def accept(f: File, n: String) = new File(f, n).isDirectory() && !MODEL_PATHS_SKIP.contains(n) })
    .flatMap(dir => readFiles(MODEL_PATH_ENCLOSURE + dir, FILE_SUFFIX_MODEL))
    .filter { case (namePrefix, _) => !MODELS_SKIP.contains(namePrefix) }
    .map(parseProg(EnclosureLegacyInterpreterType)(_))

  type ProgText = String
  type ModelName = String
    
  def parseProg(interpreterType: InterpreterType)(p: (ModelName, ProgText)): (ModelName, Prog)  = 
    p match { case (namePrefix, prog) =>
      val ast = Parser.run(Parser.prog, prog)
      val des = Desugarer(odeTransformMode = Local).run(ast)
      val apx = ApproximateRationals.run(des, interpreterType)
      (namePrefix, apx) 
    }
    
}

/**
 * Basic properties of Extract.
 * 
 * Those which involve simulation are checked using the experimental reference interpreter.
 */
object ExtractBaseTest extends Properties("ExtractHA") with ExtractTest {
    
  import progGenerator.arbProg
  
  /**
   * The transformation from Acumen to a hybrid automaton core language introduces (necessarily?)
   * a new mode variable and the transformation is thus not strictly semantics preserving.
   * However, the transformation should preserve the semantics with respect to the value of
   * continuous variables, which this property checks.
   */
  property("reference semantics preserving on continuous state (example models)") =
    existingModels.map { case (name, prog) => preservesContinuousReferenceSemanticsOf(new ExtractHA(_).res, prog, Some(name)) }.foldRight(true)(_ && _) // Make sure that all models are run
    
  property("reference semantics preserving on continous state (random models)") =
    forAll { (p: Prog) => preservesContinuousReferenceSemanticsOf(new ExtractHA(_).res, p, None) }
  
  /**
   * User mode variables are mode variables used in switches in the source model.
   * The Extract algorithm must eliminate all occurrences of these in the output model.
   */
  property("extract eliminates user mode variables") =
    forAll { (p: Prog) =>
      val umvs = userModeVariables(p)
      val extractedModel = new ExtractHA(p).res
      val varsInExtractedModel = extractedModel.defs.flatMap(c => c.priv.map(_.x.x))
      printErrUnless(!umvs.exists(varsInExtractedModel contains _),
        "\n\ntransformed: \n" + (Pretty pprint extractedModel))
    }

  // property("extract does not increase the number of modes on hybrid automaton models") =
  //   enclosureModels.forall { case (name, prog) =>
  //     val p = Desugarer().desugar(prog)
  //     val extractedModel = new extract.Extract(p).res
  //     val originalModes = countModes(p)
  //     val extractedModes = countModes(extractedModel)
  //     printErrUnless(extractedModes == originalModes,
  //       "Number of modes in transformed model " + name + " (" + extractedModes + ") exceeds number of modes in original model (" + originalModes + ")!" +
  //       "\n\ntransformed: \n" + (Pretty pprint extractedModel))
  //   }

}

/** Properties of Extract checked by simulating existing hybrid automaton models using the enclosure interpreter. */
object ExtractEnclosureTest extends Properties("ExtractHA (Enclosure Semantics)") with ExtractTest {
    
  import progGenerator.arbProg
         
  property("enclosure semantics preserving on continuous state (enclosure example models)") =
    enclosureModels.map{ case (name, prog) => preservesContinuousEnclosureSemanticsOf(new ExtractHA(_).res, prog, Some(name)) }.foldRight(true)(_&&_) // Make sure that all models are run
  
}


