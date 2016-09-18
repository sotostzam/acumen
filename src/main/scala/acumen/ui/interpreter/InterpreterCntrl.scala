package acumen
package ui
package interpreter

import scala.actors._
import scala.util.control.ControlThrowable
import java.io.File

abstract class InterpreterActor(val progText : String, val consumer : Actor) extends Actor {

  var prog : Prog = null

  // the actor may also use this method to emit optional messages to
  // the console

  def emitError(e:Throwable) = Logger.error(e)
  def emitProgressMsg(msg:String) = Logger.log(msg)

  // do not override this, see parse() and produce() instead
  
  final def act() = {
    parse()
    produce()
  }

  // Override this to parse and perform any necessary prepossessing
  // tasks.
  def parse() : Unit

  // Override this to produce results. When starting send back the
  // first bit of data (equivalent to one step) and then wait for
  // further instructions.
  def produce() : Unit
}

object InterpreterCntrl {
  // messages the Actor can expect from the consumer

  case object Flush // flush all data and wait for further instructions
  case object Stop  // exit immensely, optionally flushing all data if possible
  case object GoOn  // resume normal operation, may send data in chunks
  case object Step  // single step

  // messages the Actor may send to the consumer

  /** The boolean with the tag tells if it corresponds to a dead store (true for dead) */
  case class Chunk(tag: (Tag, Boolean),css: TraceData) // data computer so far
  case class Done(msgs: Seq[String], md: Metadata, endTime: Double) // finished computation, no more data

  // Get an appropriate InterpreterCntrl for the semantics
  // if prog is given, scan the prog for #semantics and if
  // found will override the default semantics
  def apply(default: SemanticsImpl[Interpreter], prog: Option[String] = None) : InterpreterCntrl = {
    val si = prog match {
      case Some(p) => Parser.run(Parser.getSemantics, p, None) match {
        case Some(s) => SemanticsImpl(s)
        case None => default}
      case None => default}
    si.interpreter match {
      case intr:CStoreInterpreter => new CStoreCntrl(si, intr)
      case intr:RecursiveInterpreter => new EnclosureCntrl(si, intr) 
        // fixme^: might need to be more precise
      case _ => null; // Should never happen
    }
  }
}

abstract class InterpreterCntrl {
  // Returns a new interpreter model that holds the results of the
  // computation
  def newInterpreterModel : InterpreterModel

  def semantics: SemanticsImpl[Interpreter]
  def interpreter : Interpreter

  // Creates a new actor to perform the computation.
  def init(prog: String, currentDir: File, consumer:Actor) : InterpreterActor
} 
