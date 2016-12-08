package acumen

import Errors._
import util.Names._
import acumen.util.Canonical

trait Parameters extends scala.collection.Map[String, List[InterpreterType]]
{
  def registerParm(parm: String, intr: InterpreterType)
}

object CleanParameters {

  val parms = new scala.collection.mutable.ListMap[String, List[InterpreterType]] with Parameters {
    def registerParm(parm: String, intr: InterpreterType) = 
      update(parm, intr :: get(parm).getOrElse(Nil))
  }

  // add parameters for CStore based interpreters
  acumen.interpreters.Common.simulatorFields.foreach { parm => 
    parms.registerParm(parm, acumen.TraditionalInterpreterType)
  }
  // add parameters for legacy Enclosure based interpreters
  acumen.interpreters.enclosure.Parameters.defaults.foreach {
    case (parm, _) =>
      acumen.CleanParameters.parms.registerParm(parm, acumen.EnclosureLegacyInterpreterType)
  }
  // add parameters for 2015 Enclosure based interpreters
  acumen.interpreters.enclosure2015.Common.Parameters.defaults.foreach {
    case (parm, _) =>
      acumen.CleanParameters.parms.registerParm(parm, acumen.Enclosure2015InterpreterType)
  }

  case object AssignToSimulator extends Throwable

  def filterMain(cd: ClassDef, intr: InterpreterType) : ClassDef = {
    require(cd.name == Canonical.cmain, "Applied filterMain to non-main class definition.")
    if (cd.fields.length != 1)
      throw new PositionalAcumenError {
        def mesg = "Main takes exactly one parameter."
      }.setPos(cd.pos)
    val simulatorName = cd.fields(0)

    val filter = new util.ASTMap {
      override def mapActions(actions: List[Action]) : List[Action] = actions.flatMap(filterAction(_))

      def filterAction(action: Action) : Option[Action] = action match {
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
        case _ =>
          Some(mapAction(action))
      }
    }

    try {
      filter.mapClassDef(cd)
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

