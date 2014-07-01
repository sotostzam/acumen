package acumen

import Errors._
import util.Names._

sealed abstract class InterpreterType
case object CStoreInterpreterType extends InterpreterType
case object EnclosureInterpreterType extends InterpreterType

trait Parameters extends scala.collection.Map[String, List[InterpreterType]]
{
  def registerParm(parm: String, intr: InterpreterType)
}

object CleanParameters {

  val parms = new scala.collection.mutable.ListMap[String, List[InterpreterType]] with Parameters {
    def registerParm(parm: String, intr: InterpreterType) = 
      update(parm, intr :: get(parm).getOrElse(Nil))
  }

  // add paramaters for CStore based interpreters
  acumen.interpreters.Common.simulatorFields.foreach { parm => 
    parms.registerParm(parm, acumen.CStoreInterpreterType)
  }
  // add paramters for Enclosure based interpreters
  acumen.interpreters.enclosure.Parameters.defaults.foreach {
    case (parm, _) =>
      acumen.CleanParameters.parms.registerParm(parm, acumen.EnclosureInterpreterType)
  }


  case object AssignToSimulator extends Throwable

  def filterMain(cd: ClassDef, intr: InterpreterType) : ClassDef = {
    // FIXME: make sure cd.name == "Main"
    val simulatorName = cd.fields(0)

    def filterActions(actions: List[Action]) : List[Action] = {
      actions.flatMap(filterAction(_))
    }
  
    def filterAction(action: Action) : Option[Action] = {
      action match {
        case IfThenElse(cond, t, e) =>
          Some(IfThenElse(cond, filterActions(t), filterActions(e)))
        case Switch(subject, clauses) =>
          Some(Switch(subject, clauses.map {case Clause(lhs, asrt, rhs) => 
            Clause(lhs, asrt, filterActions(rhs))}))
        case ForEach(it, col, body) =>
          Some(ForEach(it, col, filterActions(body)))
        case Discretely(Assign(lhs, rhs)) =>
          lhs match {
            case Dot(Var(Name("self",0)),sn) if sn == simulatorName =>
              throw AssignToSimulator
            case Dot(Dot(Var(Name("self",0)),sn), n) if sn == simulatorName => 
              if (parms.get(n.x).getOrElse(List(intr)).exists(_ == intr)) {
                Some(action)
              } else {
                None
              }
            case _ => 
              Some(action)
          }
        case Continuously(_) | Discretely(_) | Claim(_) | Hypothesis(_,_) =>
          Some(action)
      }
    }

    try {
      ClassDef(cd.name, cd.fields, cd.priv, filterActions(cd.body))
    } catch {
      case AssignToSimulator => 
        // FIXME: Warning to console
        println("Warning: Assign to simulator variable detected.  Can't clean parameters!")
        cd
    }
  }

  def run(t: Prog, intr: InterpreterType) : Prog = { 
    Prog(t.defs.map { cd => 
      if (cd.name.x == "Main") filterMain(cd, intr)
      else cd})
  }

}

