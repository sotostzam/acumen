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

  def emitError(e:Throwable) = App.actor ! Error(e)
  def emitProgressMsg(msg:String) = App.actor ! ProgressMsg(msg)

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

  case class Chunk(css: TraceData) // data computer so far
  case class Done(msgs: Seq[String]) // finished computation, no more data

  def cntrlForSemantics(si: SemanticsImpl[_]) : InterpreterCntrl = {
    val intr = si.interpreter()
    intr match {
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
  
  def interpreter: Interpreter

  // Creates a new actor to perform the computation.
  def init(prog: String, currentDir: File, consumer:Actor) : InterpreterActor
} 
