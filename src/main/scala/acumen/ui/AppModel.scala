package acumen
package ui

import Errors._
import util.Canonical._
import java.lang.Thread
import collection.JavaConversions._
import collection.immutable.Queue
import java.io._
import swing._
import swing.event._
import scala.actors._

sealed abstract class AppEvent extends Event
case class InterpreterChanged()  extends AppEvent
case class StateChanged()        extends AppEvent
case class Error(e:Throwable)    extends AppEvent
case class Progress(percent:Int) extends AppEvent

sealed abstract class AppState
case class Stopped()     extends AppState
case class Playing()     extends AppState
case class Paused()      extends AppState

class AppModel(text: => String) extends Publisher {
  
  private sealed abstract class PAppState
  private case class PStopped()                                extends PAppState
  private case class PInitialized(prog:Prog, store:CStore)     extends PAppState
  private case class PPlaying(p:Prog, last:CStore, c:Consumer) extends PAppState
  private case class PPaused(prog:Prog, store:CStore)          extends PAppState

  /* The currently used interpreter. Purely Functional (Reference) is used as default. */
  private var interpreter : Interpreter = interpreters.reference.Interpreter

  var isEnclosure : Boolean = false

  /** Set the currently used interpreter. */
  def setInterpreter(i:Interpreter) = {
    // TODO Figure out what the below did, and if it needs to be handled in some other way
	//interpreter.map(_.dispose) 
    interpreter = i
    var tm = interpreter.newTraceModel
    isEnclosure = tm.isInstanceOf[EnclosureTraceModel]

    if (tm == null) {
    }

    tmodel.setTraceModel(tm)

    publish(InterpreterChanged())
  }

  /* ---- state variables ---- */

  val tmodel = new TraceModelProxy(interpreter.newTraceModel)
  private var appState : PAppState = PStopped()

  /* ------ application logic --------- */

  def state : AppState = 
    appState match {
      case PStopped()        => Stopped()
      case PPaused(_,_)      => Paused()
      case PInitialized(_,_) => Paused()
      case PPlaying(_,_,_)   => Playing()
    }

  def stopEnabled =
    state match {
      case Paused() | Playing() => true
      case Stopped() => false
    }

  def playEnabled =
    state match {
      case Paused() | Stopped() => true
      case Playing() => false
    }

  def pauseEnabled = 
    state match {
      case Playing() => true
      case Paused() | Stopped() => false
    }

  def stepEnabled = 
    state match {
      case Stopped() | Paused() => true
      case Playing() => false
    }

  def codeEnabled =
    state match {
      case Stopped() => true
      case Playing() | Paused() => false
    }

  private def setState(s:PAppState) = { 
    val changed = 
      (appState, s) match {
        case (PPlaying(_,_,_)  , PPlaying(_,_,_)  ) => false
        case (PStopped()       , PStopped()       ) => false
        case (PPaused(_,_)     , PPaused(_,_)     ) => false
        case (PInitialized(_,_), PInitialized(_,_)) => false
        case _                                      => true
      }
    appState = s
    if (changed) publish(StateChanged()) 
  }
  private def emitProgress(p:Int) = publish(Progress(p))
  private def emitError(e: Throwable) = { publish(Error(e)); stop }
   
  private def withErrorReporting(action: => Unit) : Unit = {
    try action
    catch { case e => emitError(e) }
  }

  private def updateProgress(s:CStore) = {
    val time = getTime(s)
    val endTime = getEndTime(s)
    emitProgress((time*100 / endTime).toInt)
  }

  private def init : Unit = {
    tmodel.reset
    if (isEnclosure) {
      setState(PInitialized(null, null))
    } else {
      val ast = Parser.run(Parser.prog, text)
      val des = Desugarer.run(ast)
      val I = interpreter
      val (prog, store) = I.init(des)
      val cstore = I.repr(store)
      tmodel.addStore(cstore)
      setState(PInitialized(prog, cstore))
    }
  }

  def reset : Unit = {
    tmodel.reset
    setState(PStopped())
  }

  def play : Unit = withErrorReporting {
    def go(p:Prog, s:CStore) = {
      val co = new Consumer()
      val pr = new Producer(p,s,co)
      co.start
      pr.start
      setState(PPlaying(p,s,co))
    }
    appState match {
      case PStopped() =>
        init
        play
      case PInitialized(p,s) => go(p,s)
      case PPaused(p,s) => go(p,s)
      case _ => throw BadUiTransition("Can only play when Paused or Stopped.")
    } 
  }

  def pause : Unit = {
    appState match {
      case PPlaying(p,s,c) => {
        setState(PPaused(p,
          (c !? Stop) match {
            case None     => s
            case Some(ns) => ns.asInstanceOf[CStore] 
          }))
      }
      case _ => throw BadUiTransition("Can only Pause while playing.")
    }
  }

  def step : Unit = withErrorReporting {
    def go(p:Prog, s:CStore) = {
      val I = interpreter
      val mstore = I.step(p, I.fromCStore(s)) map I.repr
      mstore match {
        case Some(st) => 
          tmodel.addStore(st)
          setState(PPaused(p, st))
          updateProgress(st)
        case None =>
          setState(PStopped())
      }
    }
    appState match {
      case PPaused(p, s)      => go(p, s)
      case PInitialized(p, s) => go(p, s)
      case PStopped()         => { init; step }
      case _ => throw BadUiTransition("Step only allowed in Paused or Stopped mode.")
    }
  }

  def stop : Unit = {
    appState match {
      case PPlaying(_,_,p) => p ! Stop
      case _ => ()
    }
    setState(PStopped())
  }

  case object GoOn
  case object Stop
  case class Chunk(css:Iterable[CStore])
  case class Done(css:Iterable[CStore])
  case class EnclosureDone(tm:AbstractTraceModel)

  private class Producer(p:Prog, st:CStore, consumer:Actor) extends Actor {
    var buffer = Queue.empty[CStore]
    def handle[A](code: => A) = {
      try { code }
      catch { case e => 
        consumer ! Error(e); exit 
      }
    }
    def act : Unit = {
      if (isEnclosure) {
        val tm = interpreters.enclosure.Interpreter.generateTraceModel(text)
        consumer ! EnclosureDone(tm)
        return
      }
      val I = interpreter
      val trace = handle { I.loop(p, I.fromCStore(st)) map I.repr }
      val iter = trace.iterator
      loopWhile (iter.hasNext) {
        if (buffer.size >= 200) {
          val buf = buffer
          consumer ! Chunk(buf)
          react {
            case GoOn => buffer = Queue.empty[CStore]
            case Stop => exit
          }
        } else {
          handle { buffer = buffer enqueue iter.next }
        }
      } andThen {
        val buf = buffer
        consumer ! Done(buf)
      }
    }
  }

  private class Consumer extends Actor {
    var last : Option[CStore] = None

    def flush(css:Iterable[CStore]) = {
      val l = css.last
      Swing onEDT { 
        tmodel addStores css
        updateProgress(l)
      }
      last = Some(l)
    }

    def finish = {
      react {
        case Done(_) | Chunk(_) => 
          reply(Stop)
          exit
      }
    }

    def act = {
      loop {
        react {
          case Stop =>
            reply(last)
            finish
          case Chunk(css) =>
            reply(GoOn)
            flush(css)
          case Done(css) =>
            flush(css)
            stop
            exit
          case EnclosureDone(tm) =>
            tmodel.setTraceModel(tm)
            tmodel.fireTableStructureChanged()
            stop
            exit
          case Error(e) =>
            emitError(e)
            exit
        }
      }
    }
  }
}
