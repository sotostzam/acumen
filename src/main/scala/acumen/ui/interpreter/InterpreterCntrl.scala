package acumen
package ui
package interpreter

import scala.actors._
import scala.util.control.ControlThrowable

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

  // optionally override this to change how the program is parsed

  def parse() = {
    val ast = Parser.run(Parser.prog, progText)
    val dif = SD.run(ast)
    val des = Desugarer().run(dif)
    val des2 = if (Main.extractHA)
                 new Extract(des).res
               else
                 des
    prog = des2
  }

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
  case object Done // finished computation, no more data

  def cntrlForInterpreter(intr: Interpreter) : InterpreterCntrl = {
    intr match {
      case intr:CStoreInterpreter => new CStoreCntrl(intr)
      case intr:RecursiveInterpreter => new EnclosureCntrl(intr) 
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
  def init(prog: String, consumer:Actor) : InterpreterActor
}
