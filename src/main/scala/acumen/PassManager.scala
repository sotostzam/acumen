package acumen

import Errors._

object PassManager {

  case class Pass(id: String, desc: String, trans: Prog => Prog, category: String, var idx: Int = -1)
  def mkPass(id: String, desc: String, trans: Prog => Prog, category: String = null)
    = Pass(id, desc, trans, if (category == null) id else category)
  val availPasses = Array(
    // Order matters!  The order is the order the passes are applied.
    // And each passes is grouped into mutually excursive categories
    // in which only one pass from that category is applied.
    mkPass("BTA", "Binding time analysis", BindingTimeAnalysis.run(_)),
    mkPass("SD", "Symbolic differentiation", SD.run(_)),
    mkPass("toposort", "Topo. Sort. Priv Section", passes.TopoSortInit.proc(_)),
    mkPass("inlinepriv", "Inline Priv Deps.", passes.InlineInitDeps.proc(_)),
    mkPass("flatten", "Object Flattening (Simple Version)", passes.FlattenSimple.run(_)),
    mkPass("elimconst", "Eliminate Constants (Single objects only)", passes.ElimConst.proc(_)),
    mkPass("extract-ha", "H.A. Extraction", new passes.ExtractHA(_,Main.debugExtract).res),
    mkPass("killnot", "Kill Nots", passes.KillNot.mapProg(_)),
    mkPass("desugar-toplevel", "Desugarer (Top Level)", Desugarer(odeTransformMode=TopLevel).run(_), category="desugar"),
    mkPass("desugar-local", "Desugarer (Local)", Desugarer(odeTransformMode=Local).run(_), category="desugar"),
    mkPass("desugar-local-inline", "Desugarer (Local Inlining)", Desugarer(odeTransformMode=LocalInline).run(_), category="desugar"),
    mkPass("typecheck", "Type Checker", {prog => 
                                         val (typechecked, res) = new TypeCheck(prog).run()
                                         println("\nTYPE CHECK RESULT: " + TypeCheck.errorLevelStr(res) + "\n")
                                         typechecked})
  )
  case class PassAlias(from: String, to: Seq[String], desc: Option[String]) 
  // ^ If desc is none the alias won't show up in help screens
  val passAliases = Seq(
    PassAlias("sd", Seq("SD"), None),
    PassAlias("BTA",Seq("BTA"), Some("enable symolic differentiation and acasual equations")),
    PassAlias("extract", Seq("extract-ha"), None),
    PassAlias("normalize", Seq("toposort", "inlinepriv", "elimconst", "extract-ha", "killnot"),
              Some("Normalize the program into a H.A.")))
  availPasses.indices.foreach{i => availPasses(i).idx = i}
  val passLookup : Map[String,Seq[Pass]] = {
    val m = availPasses.map{v => (v.id,Seq(v))}.toMap
    m ++ passAliases.map{v => (v.from, v.to.flatMap{m(_)})}
  }

  // applyPasses: Takes in a prog and a list of passes to apply.
  // The special pass "nodefaults" suppress the default passes from
  //   being applied; this can only be specified as part of 
  //   "required".
  // The passes will be applied in a fixed order determined by the order
  //  in which they appear in availPasses (i.e. the order in which
  //  they are specified in args is irrelevant)
  def splitPassesString(str: String) : Seq[String] = if (str == "") Nil else str.split(',')
  def applyPasses(p: Prog, required: Seq[String], defaults: Seq[String], extraPasses: Seq[String], interpreterType: InterpreterType) : Prog = {
    val (nodefaults, rest) = required.partition(_ == "nodefaults")
    val passList : Seq[String] = ((if (nodefaults.isEmpty) defaults else Nil) 
                                  ++ extraPasses
                                  ++ rest)
    val passes = passList.flatMap{s => passLookup.get(s) match {
      case Some(pass) => pass; case None => throw UnrecognizedTransformation(s)
    }}.groupBy{_.category}.map{_._2.last}. // only take the last pass specified for each category
       toSeq.sortWith{(a,b) => a.idx < b.idx} // sort by the orignal order in availPasses
    var res = p
    //println("PASSES: " + passes.map{pass => pass.id}.mkString(" "))
    passes.foreach{pass => res = pass.trans(res)}
    ApproximateRationals.run(res, interpreterType)
  }
  def validatePassesStr(args0: String*) : Unit = {
    val args = args0.flatMap(_.split(',')).toList
    args.foreach{arg => passLookup.get(arg) match {
      case Some(pass) => /* do nothing */
      case None if arg == "" => /* do nothing */
      case None => throw UnrecognizedTransformation(arg)
    }}
  }
}
