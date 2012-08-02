import java.io.InputStreamReader

import acumen._ // datatypes
import acumen.Pretty._ // pretty printer
import acumen.Errors._ // exceptions
import acumen.util.Names._ // for 'name'
import acumen.util.Canonical._ // store manipulation functions
import acumen.util.Conversions._ // values extractors
import acumen.interpreters.reference.Interpreter // purely functional interpreter

object Test {

  def str(s:String) = VLit(GStr(s))
  def double(d:Double) = VLit(GDouble(d))

  sealed abstract class Action
  case class NoAction()        extends Action
  case class Inject(q:Double)  extends Action
  case class Glucose(q:Double) extends Action

  def transform(st: CStore, decider: Double => Action) : CStore = {
    try {
      val gmmId = single(ClassName("GlucoseMinimalModel"))(st)
        val g = extractDouble(getObjectField(gmmId, name("g"), st))
        decider(g) match {
        case Inject(q) =>
        println("inject " + q)
        val st1 = setObjectField(gmmId, name("mode"), str("INJECT"), st)
          setObjectField(gmmId, name("insulin"), double(q), st1)
        case Glucose(q) =>
        println("glucose " + q)
        val st1 = setObjectField(gmmId, name("mode"), str("INJECT"), st)
          setObjectField(gmmId, name("glucose"), double(q), st1)
        case NoAction() => 
        st
      }
    } catch {
      case NoInstanceFound(_) => st 
    }
  }

  def decide(g: Double) : Action = {
    if (g<10)
      Inject(3)
    else if (g>20)
      Glucose(3)
    else
      NoAction()
  }

  def main(args: Array[String]) : Unit = {

    val in  = new InputStreamReader(System.in)
  	val ast = Parser.run(Parser.prog, in)
    val desugared = Desugarer.run(ast)
    val (prog, st) = Interpreter.init(desugared)

    // main loop definition
    def run(st: CStore) : Unit = {
      println(pprint(prettyStore(st)))
      println("-"*30)
      Interpreter.step(prog, transform(st, decide)) match {
        case Some(newst) => run(newst)
        case None        => ()
      }
    }
    
    // run main loop
    run(st)
  }
}
