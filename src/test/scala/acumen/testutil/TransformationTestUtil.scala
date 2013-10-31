package acumen
package testutil

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import acumen.Action
import acumen.CStoreRes
import acumen.ClassName
import acumen.Continuously
import acumen.Desugarer
import acumen.Equation
import acumen.EquationI
import acumen.EquationT
import acumen.ForEach
import acumen.IfThenElse
import acumen.Name
import acumen.Pretty.pprint
import acumen.Prog
import acumen.Switch
import acumen.VClassName
import acumen.Var
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure

/** Utilities for testing Prog-to-Prog transformations. */
object TransformationTestUtil {
  
  /** 
   * Given a Prog p, computes its desugared version d. Checks that the simulation trace
   * of d is the same as that obtained by first applying transform to d and then simulating.
   */
  def preservesContinuousReferenceSemanticsOf(transform: Prog => Prog, p: Prog, modelName: Option[String]) =
    preservesSemanticsOf(transform, p, modelName, "continuous non-rigorous", { (desugared, transformed) =>
      val contNames = getContinuousVariables(p)
      // set up the experimental interpreter
      val i = interpreters.reference.experimental.Interpreter
      // run desugared program before transformation
      val dtrace = i.run(desugared)
      dtrace.ctrace.last // force evaluation
      // run desugared program after transformation
      val etrace = i.run(transformed)
      etrace.ctrace.last // force evaluation
      // compare pretty-printed traces
      val (dcs, ecs) = (onlyContinuousState(dtrace, contNames), onlyContinuousState(etrace, contNames))
      val (ds, es) = (dumpSampleToString(dcs), dumpSampleToString(ecs))
      ds == es
    })

  /**
   * Given a Prog p, computes its desugared version d. Checks that the part of the enclosure simulation trace
   * of d that corresponds to continuous variables is the same as that obtained by first applying transform 
   * to d and then simulating.
   *
   * FIXME: Currently only works on models containing a single class (Main).
   */
  def preservesContinuousEnclosureSemanticsOf(transform: Prog => Prog, p: Prog, modelName: Option[String]) =
    preservesSemanticsOf(transform, p, modelName, "enclosure", { (desugared, transformed) =>
      val originalEnclosures = acumen.interpreters.enclosure.Interpreter.run(desugared).res
      val transformedEnclosures = acumen.interpreters.enclosure.Interpreter.run(transformed).res
      areEqual("Enclosure counts", originalEnclosures.length, transformedEnclosures.length) && {
        val contNames = getContinuousVariables(p)
        originalEnclosures.zip(transformedEnclosures).forall {
          case (orig, trans) => //FIXME Update w.r.t. multi-object models
            equalOnComponents(orig, trans, varName => !contNames.getOrElse(ClassName("Main"), Set.empty).contains(Name(varName, 0)))
        }
      }
    })
  
  /**
   * Given a Prog p, computes its desugared version d. Using comparator, checks that the enclosure simulation trace
   * of d is the same as that obtained by first applying transform to d and then simulating.
   */
  def preservesSemanticsOf(transform: Prog => Prog, p: Prog, modelName: Option[String], semanticsType: String, comparator: (Prog,Prog) => Boolean) = {
    var same = false
    val desugared = Desugarer.run(p)
    var transformed : Prog = null
    try {
      transformed = transform(desugared)
      same = comparator(desugared, transformed)
      print (if (same) "+" else "-") 
      same
    } catch {
      case e =>
        e.printStackTrace
        throw e
    } finally if (!same) {
      System.err.println("\n Transform is not (" + semanticsType + ") semantics preserving.\n\n")
      modelName.foreach(mn => System.err.println("\nFailing model: " + mn + "\n"))
      System.err.println(
        "\nraw: \n"       + pprint(p) +
        "\ndesugared: \n" + pprint(desugared))
      printErrUnless(transformed != null,
        "\n\ntransformed: \n" + pprint(transformed))
    }
  }
  
  def dumpSampleToString(csr: CStoreRes): String = {
    val baos = new ByteArrayOutputStream
    csr.dumpContinuous(new PrintStream(baos))
    baos.toString
  }

  /** Print message to stderr if cond is false. Returns cond. */
  def printErrUnless(cond: Boolean, message: String) = {
    if (!cond) System.err.println(message)
    cond
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
   * as the className variable of each object. 
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
    
  type VarName = String
    
  /** Checks that two UnivariateAffineEnclosures are equal with respect to a subset of their components. */
  def equalOnComponents( left: UnivariateAffineEnclosure
                       , right: UnivariateAffineEnclosure
                       , characteristicFunction: VarName => Boolean) =
    areEqual("Enclosure domains", left.domain, right.domain) &&
      areEqual("Enclosure component sizes ", left.components.size, right.components.size) && {
        val leftCompSubset = left.components.filterKeys(characteristicFunction)
        val rightCompSubset = right.components.filterKeys(characteristicFunction)
        leftCompSubset.forall {
          case (varName, enclosure) => 
            areEqual("Enclosure component " + varName, enclosure.toString, rightCompSubset.getOrElse(varName, "None").toString)
        }
      }
    
  /** Returns true of expected == observed, otherwise prints an error message and returns false. */
  def areEqual[T](description: String, expected: T, observed: T) = 
    if (expected == observed)
      true
    else { 
      System.err.println(description + " mismatch. Expected: " + expected + ", observed: " + observed)
      false
    }
    
  /** 
   * Returns the number of modes in prog.
   * The prog must be in hybrid automaton form (i.e. single class containing a single switch statement, among other things).  
   */
  def countModes(prog: Prog): Int = {
    // Check that prog is on hybrid automaton form
    new interpreters.enclosure.Checker{}.checkValidAutomatonEmbedding(prog.defs(0))
    prog.defs(0).body.map(_ match {
      case Switch(_,cs) => cs.map(_.lhs).toSet.size // Collapse clauses with identical lhs
      case _ => 0
    }).sum
  }

  /** Returns the list of variables used to switch between case clauses in prog. */
  def userModeVariables(prog: Prog): List[Name] = {
    def userModeVariables(a: Action): Set[Name] = a match {
      case Switch(Var(n), cs) =>
        (for { c <- cs; a <- c.rhs; v <- userModeVariables(a) } yield v).toSet + n
      case Switch(Dot(_, n), cs) =>
        (for { c <- cs; a <- c.rhs; v <- userModeVariables(a) } yield v).toSet + n
      case IfThenElse(_, t, e) =>
        (t.flatMap(userModeVariables) ++ e.flatMap(userModeVariables)).toSet
      case _ => Set.empty
    }
    prog.defs.flatMap(_.body flatMap userModeVariables)
  }
  
}



