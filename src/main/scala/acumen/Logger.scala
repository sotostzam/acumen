package acumen

import scala.util.parsing.input.{Position,NoPosition}
import Errors.PositionalAcumenError
import scala.collection.mutable.ArrayBuffer

class Logger {
  import Logger._

  private val appenders = new ArrayBuffer[Appender]
  private val buffer = new ArrayBuffer[Instruction]

  /** Attach an appender to this logger.  For example when
    * the U.I is active it will likely be the console. */
  def attach(a: Appender) { this.synchronized{
    appenders += a
    buffer.foreach { instr => a(instr); }
    buffer.clear()
  }}
  /** Detatch all appenders, messages will be queued until an appender is attached */
  def detachAll() { this.synchronized{
    appenders.clear
  }}
  /** Detatch appender a. If no appenders are left, messages will be queued until an appender is attached */
  def detach(a: Appender) { this.synchronized{
    appenders -= a
  }}
  /** Append a message or instruction. */
  def append(instr: Instruction) { this.synchronized {
    if (appenders isEmpty)
      buffer += instr
    else for (a <- appenders) a(instr)
  }}

  /** Convenience method: Send a error message from a Throwable object */
  def error(e: Throwable) : Unit = e match {
    case pe: PositionalAcumenError => append(Message(ERROR, ExceptionMsg(e), pe.pos))
    case _                         => append(Message(ERROR, ExceptionMsg(e), NoPosition))
  }
  /** Convenience method: Send a error message */
  def error(msg: String, pos: Position = NoPosition) = 
    append(Message(ERROR, TextMsg(msg), pos))

  /** Convenience method: Send a status update */
  def status(dotsBefore: Boolean, msg: String, dotsAfter: Boolean) =
    append(StatusUpdate(dotsBefore, msg, dotsAfter))

  /** Convenience method: Send an informative message. Visible to users. */
  def log(msg: String) =
    append(Message(INFO, TextMsg(msg)))
    
  /** Convenience method: Send a medium-level message. Only visible to developers. */
  def debug(msg: String) =
    append(Message(DEBUG, TextMsg(msg)))
    
  /** Convenience method: Send a low-level message. Only visible to developers. */
  def trace(msg: String) =
    append(Message(TRACE, TextMsg(msg)))

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
  sealed abstract class Level(val orderNum: Int)
  case object TRACE  extends Level(0)
  case object DEBUG  extends Level(1)
  case object INFO   extends Level(2)
  case object WARN   extends Level(3)
  case object ERROR  extends Level(4)
  val levels = List(TRACE, DEBUG, INFO, WARN, ERROR)

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
  sealed abstract class Msg extends Instruction {val level:Level}
  /** A normal message. */
  case class Message(level: Level, message: MsgContent, pos: Position = NoPosition) extends Msg
  /** A normal message as an exception (i.e., with Backtrace information) */
  case class HypothesisReport(md: SomeMetadata, startTime: Double, endTime: Double) extends Msg {val level = INFO}

  sealed abstract class MsgContent {val msg: String; override def toString = msg;}
  case class TextMsg(msg: String) extends MsgContent 
  case class ExceptionMsg(exception: Throwable) extends MsgContent {override val msg = exception.getMessage();}
  
  abstract class Appender {
    def apply(instr: Instruction)
  }
}
