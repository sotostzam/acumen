package acumen
package interpreters
package compiler2015

import java.io.{
  File, FileInputStream, PrintWriter 
}
import scala.util.parsing.input.NoPosition

import Common._
import Desugarer._
import Errors._
import util.ASTUtil._
import util.Canonical._
import util.Conversions._
import util.Random

abstract class StateKind
case object Continuous extends StateKind
case object Discrete extends StateKind

abstract class Compiler extends CStoreInterpreter {
  
  /* Compiler */
  
  val prefix = "acumen/interpreters/compiler2015/"
  
  /* Abstract members */
  
  /** Program file suffix for the target language */
  def suffix: String
  /** Discrete state array name */
  def discreteStateName: String
  /** Continuous state array name */
  def continuousStateName: String

  /** Generate a function to look up variable names for continuous state variables */
  def genVariableName(ir: IR): String
  /** Generate a function to look up string variable values */
  def genStringValues(ir: IR): String
  /** Generate a function that returns the initial store */
  def genInitialStore(ir: IR): String
  /** Generate a function that computes a discrete step */
  def genDiscreteStep(ir: IR): String
  /** Generate a function that computes and evaluates the active field */
  def genField(ir: IR): String
  
  /** Generate a ground value  */
  def genGroundValue(gv: GroundValue)(implicit ir: IR, sk: StateKind): String
  /** Generate an operation */
  def genOp(f:Name, es:List[Expr])(implicit ir: IR, sk: StateKind): String  
  /** Generate an array access*/
  def genArrayAccess(arrayName: String, index: Int, ir: IR): String
  
  /* Generic members */
  
  def toIR(prog: Prog): IR = IR(prog, "")
  
  /** Generic compiler. Uses includes an abstract members to generate a complete program. */
  def gen(ir: IR): String =
    List( readPart("prelude")
        , genVariableName(ir)
        , genStringValues(ir)
        , genInitialStore(ir)
        , genDiscreteStep(ir)
        , genField(ir)
        , readPart("main") 
        ).mkString("\n")

  /** Generate an expression */
  def genExpr(e: Expr)(implicit ir: IR, sk: StateKind): String =
    e match {
      case Lit(i) => genGroundValue(i)
      case Dot(o, f) =>
        ir.continuousState.zipWithIndex.find { case (((cid, name), _), i) => name == f }
          .map { case (_, index) => genArrayAccess(continuousStateName, index, ir) }
          .getOrElse(
            ir.discreteState.zipWithIndex.find { case (((cid, name), _), i) => name == f }
              .map { case (_, index) => genArrayAccess(discreteStateName, index, ir) }
              .get)
      case Index(e, i) => genExpr(e) + parens((i map genExpr).mkString(", "))
      case ResolvedDot(id, _, f) =>
        ir.continuousState.zipWithIndex.find { case (((cid, name), _), i) => cid == id && name == f }
          .map { case (_, index) => genArrayAccess(continuousStateName, index, ir) }
          .getOrElse(
            ir.discreteState.zipWithIndex.find { case (((cid, name), _), i) => cid == id && name == f }
              .map { case (_, index) => genArrayAccess(discreteStateName, index, ir) }
              .get)
      case Op(f, es)            => genOp(f, es)
      case ExprVector(l)        => parens((l map genExpr).mkString(", "))
      case TypeOf(cn)           => "type" + parens(genClassName(cn))
      case ExprInterval(lo, hi) => brackets(genExpr(lo) + " .. " + genExpr(hi))
      case ExprIntervalM(m, r)  => parens(genExpr(m) + "+/-" + genExpr(r))
      case Pattern(l) => l match {
        case n :: Nil => genExpr(n)
        case _        => parens((l map genExpr).mkString(", "))
      }
    }

  /** Generate a class name */
  def genClassName(n: ClassName)(implicit ir: IR, sk: StateKind): String =
    n.x

  /** Generate a variable name */
  def genName(n: Name): String = 
    n.x + "'" * n.primes

  /** Generate a conjunction of the boolean expressions in p */
  def genConjunction(p: Set[Expr], ir: IR, sk: StateKind): String =
    if (p isEmpty) "true"
    else p.map(genExpr(_)(ir, sk)).mkString(" && ")
    
  /* CStoreInterpreter */
  
  type Store = CStore

  def init(prog: Prog): (Prog, CStore, Metadata) = {
    import Compiler._
    val ir = toIR(prog)
    printToFile(gen(ir), None, suffix)
    (prog, ir.initialCStore, NoMetadata)
  }
    
  def step(p: Prog, st: CStore, md: Metadata): StepRes =
    Done(md, getEndTime(st))

  def visibleParameters(): Map[String, CValue] = Map.empty

  def repr(s: CStore): CStore = s

  def fromCStore(cs: CStore, root: CId): Store = cs
    
  /* Utilities */
    
  def brackets(s: String) = "[" + s + "]"
  def parens(s: String) = "(" + s + ")"
  def readPart(name: String) = 
    io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(prefix + name + "." + suffix + ".part")).mkString

}

object Compiler {
  
  def printToFile(data: String, fileName: Option[File], suffix: String) {
    val p = new PrintWriter(fileName.getOrElse(new File("acumen." + suffix)))
    try { p println data } finally { p.close() }
  }
  
}