package acumen
package testutil

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import acumen.Pretty.pprint

/** Utilities for testing Prog-to-Prog transformations. */
object TransformationTestUtil {
  
  /** 
   * Given a Prog p, computes its desugared version d. Checks that the simulation trace
   * of d is the same as that obtained by first applying transform to d and then simulating.
   */
  def preservesContinousSemanticsOf(transform: Prog => Prog, p: Prog, modelName: Option[String]) = {
    var same = false
    val desugared = Desugarer.run(p)
    var transformed : Prog = null
    try {
      transformed = transform(desugared)
      val contNames = getContinuousVariables(p)
      // set up the newreference interpreter
      val i = interpreters.newreference.Interpreter
      // run desugared program before transformation
      val dtrace = i.run(desugared)
      dtrace.ctrace.last // force evaluation
      // run desugared program after transformation
      val etrace = i.run(transformed)
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
      modelName.foreach(mn => System.err.println("\nFailing model: " + mn + "\n"))
      System.err.println("\nraw: \n")
      System.err.println(pprint(p))
      System.err.println("\ndesugared: \n")
      System.err.println(pprint(desugared))
      if (transformed != null) {
        System.err.println("\n\ntransformed: \n")
        System.err.println(pprint(transformed))
      }
    }
  }
  
  def dumpSampleToString(csr: CStoreRes): String = {
    val baos = new ByteArrayOutputStream
    csr.dumpContinuous(new PrintStream(baos))
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
  
}



