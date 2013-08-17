package acumen

import org.scalacheck._
import Gen._
import Shrink._
import Arbitrary.arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop._
import acumen.Pretty.pprint
import testutil.ProgGenerator.{
  arbProg
}
import acumen.util.System.{
  readFiles, FILE_SUFFIX_MODEL
}
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.io.File
import java.io.FilenameFilter

import scala.util.matching.Regex

object ExtractTest extends Properties("Extract") {

  val MODEL_PATH_SINGLE_CLASS = "examples/XXX_internal/one-class"
  val MODEL_PATH_ENCLOSURE    = "examples/9_Experimental/01_Enclosures/"
    
  /**
   * The transformation from Acumen to a hybrid automaton core language 
   * introduces (necessarily?) 
   * additional mode variables and the transformation is thus not strictly semantics preserving. 
   * However, the transformation should preserve the semantics with respect to the value of 
   * continuous variables, which this property checks.
   */
  property("semantics preserving on continous state (random models)") =
    forAll { (p: Prog) => preservesContinousSemanticsOf(p) }

  property("semantics preserving on continuous state (example models)") =
    existingModels.forall(preservesContinousSemanticsOf)

  /** Load models compatible with the transformation from the examples directory. */
  //TODO Update when support for multi-object models is added to Extract
  def existingModels(): Iterable[Prog] =
    (readFiles(MODEL_PATH_SINGLE_CLASS, FILE_SUFFIX_MODEL) ++
     new File(MODEL_PATH_ENCLOSURE).list(new FilenameFilter ()
       { def accept(f: File, n: String) = new File(f,n).isDirectory() })
       .flatMap(dir => readFiles(MODEL_PATH_ENCLOSURE + dir, FILE_SUFFIX_MODEL)))
    .map { case (_, prog:String) => Parser.run(Parser.prog, prog) }
  
  /**
   * Given a Prog p, computes its desugared version d. Checks that the simulation trace
   * of d is the same as that obtained by first applying Extract to d and then simulating.
   */
  def preservesContinousSemanticsOf(p: Prog) = {
    var same = false
    val desugared = Desugarer.run(p)
    var extracted : Prog = null
    try {
      extracted = new Extract(desugared).res
      val contNames = getContinuousVariables(p)
      // set up the reference interpreter
      val i = interpreters.reference.Interpreter
      // run desugared program before transformation
      val dtrace = i.run(desugared)
      dtrace.ctrace.last // force evaluation
      // run desugared program after transformation
      val etrace = i.run(extracted)
      etrace.ctrace.last // force evaluation
      // compare pretty-printed traces
      val (dcs, ecs) = (onlyContinuousState(dtrace, contNames), onlyContinuousState(etrace, contNames))
      val (ds, es)  = (dumpSampleToString(dcs), dumpSampleToString(ecs))
      same = ds == es
      print (if (same) "+" else "-") 
      same
    } catch {
      case e =>
        e.printStackTrace
        throw e
    } finally if (!same) {
      System.err.println("\ndesugared: \n")
      System.err.println(pprint(desugared))
      if (extracted != null) {
        System.err.println("\n\nextracted: \n")
        System.err.println(pprint(extracted))
      }
    }
  }
  
  def dumpSampleToString(csr: CStoreRes): String = {
    val baos = new ByteArrayOutputStream
    csr.dumpSample(new PrintStream(baos))
    baos.toString
  }

  /** Filters away any irrelevant variables from the input CStoreRes. */
  def onlyContinuousState(csr: CStoreRes, contNames: Map[ClassName, Set[Name]]): CStoreRes =
    CStoreRes(for { st <- csr.ctrace } yield onlyContinuousState(st, contNames))
  
  /** Filters away any irrelevant variables from a CStore. */
  def onlyContinuousState(cst: CStore, contNames: Map[ClassName, Set[Name]]): CStore =
    (for { (id, obj) <- cst ; VClassName(className) = obj(Name("className",0)) } 
      yield (id, for { (name, value) <- obj; if keep(name, className, contNames) } 
        yield (name, value) )
    ).toMap
  
  /**
   * Determines if the varName should be taken into account in the test.
   * This includes continuous variables, fields of the Simulator object as well
   * as the className varaible of each object. 
   */
  def keep(varName: Name, className: ClassName, contNames: Map[ClassName, Set[Name]]) =
    className.x == "Simulator" || varName.x == "className" ||
      (contNames.getOrElse(className, Set.empty) contains varName) 

  /**
   * Returns a Seq of variables Name(n, primes) for which Prog contains a continuous assignment to a
   * variable with Name(n, ps) for any ps. For example, if the Prog contains a continuous assignment
   * to Name(x,2), the returned Seq will contain at least { Name(x,0), Name(x,1), Name(x,2) }.   
   */
  def getContinuousVariables(p: Prog): Map[ClassName, Set[Name]] =
    p.defs.flatMap { classDef =>
      Stream.continually(classDef.name).zip(
      (classDef.fields ++ classDef.priv.map(_.x))
      .filter(name => classDef.body.exists(containsContinuousAssignemtTo(name, _))))
    }.groupBy(_._1).mapValues(_.map(_._2).toSet)
  
  /** Returns true if a or any of its sub-statements contains a continuous assignment to n. */
  def containsContinuousAssignemtTo(n: Name, a: Action): Boolean = a match {
    case Continuously(EquationI(Var(name), _)) => name.x == n.x
    case Continuously(EquationT(Var(name), _)) => name.x == n.x
    case Continuously(Equation(Var(name), _))  => name.x == n.x
    case IfThenElse(_, t, e)                   => t.exists(containsContinuousAssignemtTo(n, _)) ||
                                                  e.exists(containsContinuousAssignemtTo(n, _))
    case Switch(_, cs)                         => cs.exists(_.rhs.exists(containsContinuousAssignemtTo(n, _)))
    case ForEach(_,_,as)                       => as.exists(containsContinuousAssignemtTo(n, _))
    case _                                     => false
  }
  
} 
