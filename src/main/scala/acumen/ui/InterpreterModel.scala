package acumen
package ui

import scala.actors._
import scala.util.control.ControlThrowable

abstract class InterpreterActor(val progText : String, val consumer : Actor) extends Actor {

  var prog : Prog = null

  // the actor may also use this method to emit optional progress to
  // the console

  def emitError(e:Throwable) = Acumen.actor ! Error(e)
  def emitProgressMsg(msg:String) = Acumen.actor ! ProgressMsg(msg)

  // do not override this, see parse() and produce() instead
  
  final def act() = {
    parse()
    produce()
  }

  // optionally override this to change how the program is parsed

  def parse() = {
    val ast = Parser.run(Parser.prog, progText)
    val dif = SD.run(ast)
    val des = Desugarer.run(dif)
    prog = des
  }

  // override this to produce results, but first send an empty chunk
  // and wait for further instructions
  
  def produce() : Unit
}

object InterpreterModel {
  // messages the Actor can expect from the consumer
  
  case object Flush // flush all data and wait for further instructions
  case object Stop  // exit immensely, optinally flushing all data if possible
  case object GoOn  // resume normal operation, may send data in chunks
  case object Step  // single step

  // messages the Actor may send to the consumer

  case class Chunk(css: TraceData) // data computer so far
  case object Done // finished computation no more data
}

abstract class InterpreterModel {
  // Returns a new trace model that holds the results of the
  // computation
  def newTraceModel : AbstractTraceModel

  // Creates a new actor to perform the computation
  def init(prog: String, consumer:Actor) : InterpreterActor
}
