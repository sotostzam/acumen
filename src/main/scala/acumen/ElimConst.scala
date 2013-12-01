package acumen

import acumen.extract.Util.getName
import scala.collection.mutable.{HashSet => MutSet}

object ElimConst {

  def proc(p: Prog) : Prog = {
    if (p.defs.size > 1) 
      throw acumen.extract.OtherUnsupported("Multiple objects not supported.")
    if (p.defs(0).name != ClassName("Main"))
      throw acumen.extract.OtherUnsupported("Could not find Main class.")
    Prog(List(elimConst(p.defs(0))))
  }

  private def elimConst(cd: ClassDef) : ClassDef = {
    var candidates = cd.priv.collect{case Init(name, ExprRhs(v)) if extract.Util.extractDeps(v).isEmpty => (name, v)}.toMap
    var kill = new MutSet[String]
    def killIt(n: Name) {kill += n.x}
    new Visitor {
      override def visitContinuousAction(a: ContinuousAction) : Unit = a match {
        case Equation(lhs, rhs) => {getName(lhs).foreach{n => killIt(n)}}
        case EquationI(lhs, rhs) => {getName(lhs).foreach{n => killIt(n)}}
        case EquationT(lhs, rhs) => {getName(lhs).foreach{n => killIt(n)}}
      }
      override def visitDiscreteAction(a: DiscreteAction) : Unit = a match {
        case Assign(lhs, rhs) => {getName(lhs).foreach{n => killIt(n)}}
      }
    }.visitClassDef(cd)
    var consts : Map[Name,Expr] = candidates.filter{case (Name(n,_),_) => !kill.contains(n)}
    val init = cd.priv.filter{case Init(n, _) if (consts.contains(n)) => false; case _ => true}
    val body = new ASTMap {
      override def mapExpr(e: Expr) : Expr = {
        getName(e) match {
          case Some(n) => consts.get(n) match {
            case Some(v) => v
            case None => e
          }
          case None => super.mapExpr(e)
        }
      }
    }.mapActions(cd.body)
    ClassDef(cd.name, cd.fields, init, body)
  }
}
