package acumen

import scala.util.parsing.input.{Position,NoPosition}
import Errors.PositionalAcumenError
import scala.collection.mutable.ArrayBuffer

class Logger {
  import Logger._

  private var appender: Option[Appender] = None
  private var buffer = new ArrayBuffer[Instruction]

  /** Attach an appender to this logger.  For example when
    * the U.I is active it will likely be the console. */
  def attach(a: Appender) { this.synchronized{
    appender = Some(a)
    buffer.foreach { instr => a(instr); }
    buffer.clear()
  }}
  /** Detatch the appender, messages will be queued until a reciver is attached */
  def detach() { this.synchronized{
    appender = None
  }}
  /** Append a message or instruction. */
  def append(instr: Instruction) { this.synchronized {appender match {
    case Some(a) => a(instr)
    case None    => buffer += instr
  }}}

  /** Convenience method: Send a error message from a Throwable object */
  def error(e: Throwable) : Unit = e match {
    case pe: PositionalAcumenError => error(pe.getMessage, pe.pos)
    case _                         => error(e.getMessage)
  }
  /** Convenience method: Send a error message */
  def error(msg: String, pos: Position = NoPosition) = 
    append(Message(ERROR, msg, pos))

  /** Convenience method: Send a status update */
  def status(dotsBefore: Boolean, msg: String, dotsAfter: Boolean) =
    append(StatusUpdate(dotsBefore, msg, dotsAfter))

  /** Convenience method: Send a informative message. */
  def log(msg: String) =
    append(Message(INFO, msg))

  /** Convenience method: Send a hypothesis report if there is SomeMetedata. */
  def hypothesisReport(md: Metadata, startTime: Double, endTime: Double) = md match {
    case sm:SomeMetadata => append(HypothesisReport(sm, startTime, endTime))
    case _               =>
  }
  
  /** Convenience method: Separate new messages from old. */
  def separator() = append(Separator)
}

object Logger extends Logger {
  /** Various log levels */
  sealed abstract class Level(orderNum: Int)
  case object TRACE  extends Level(0)
  case object DEBUG  extends Level(1)
  case object INFO   extends Level(2)
  case object WARN   extends Level(3)
  case object ERROR  extends Level(4)

  /** Internal level to use for status updates. Do not use directly. */
  case object STATUS extends Level(2)

  /** Instructions that can be sent to the Logger */
  sealed abstract class Instruction
  /** An status update.  For example:
   *  StatusUpdate(false, "Starting", true),
   *  StatusUpdate(true, "Stopped", false) */
  case class StatusUpdate(dotsBefore: Boolean, message: String, dotsAfter: Boolean) extends Instruction
  /** Separate new messages from old ones some how.  For the console
   * this means dimming old messages. */
  case object Separator extends Instruction
  /** Some sort of complete message. */
  sealed abstract class Msg extends Instruction
  /** A normal message. */
  case class Message(level: Level, message: String, pos: Position = NoPosition, backTrace: Option[Throwable] = None) extends Msg
  /* A HypothesisReport */
  case class HypothesisReport(md: SomeMetadata, startTime: Double, endTime: Double) extends Msg
  
  abstract class Appender {
    def apply(instr: Instruction)
  }
}
