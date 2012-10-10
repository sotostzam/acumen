package acumen
package ui

import Errors._

import util.Canonical._
import util.Conversions._

import java.lang.Thread
import collection.JavaConversions._
import collection.immutable.Queue
import java.io._
import swing._
import swing.event._
import scala.actors._
import scala.util.control.ControlThrowable

import collection.mutable.ArrayBuffer
import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.EnclosureInterpreterCallbacks

sealed abstract class AppEvent extends Event
case class InterpreterChanged()  extends AppEvent
case class StateChanged() extends AppEvent
case class Error(e: Throwable) extends AppEvent
case class Progress(percent: Int) extends AppEvent
case class ProgressMsg(msg:String) extends AppEvent
case class Progress3d(percent: Int) extends AppEvent
//case class Playing3d() extends AppEvent

sealed abstract class AppState
case class Stopped() extends AppState
case class Playing() extends AppState
case class Paused() extends AppState

sealed abstract class InterpreterType
case class Pure() extends InterpreterType
case class Impure(nbThreads: Int) extends InterpreterType

class AppModel(text: => String, console: Console) {

  private sealed abstract class PAppState
  private case class PStopped() extends PAppState
  private case class PInitialized(prog: Prog, store: CStore) extends PAppState
  private case class PPlaying(p: Prog, last: CStore, c: Consumer) extends PAppState
  private case class PPaused(prog: Prog, store: CStore) extends PAppState

  //*************************
  var data = new ThreeDData(console);

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
    tmodel.setTraceModel(tm)

    Acumen.actor ! InterpreterChanged()
  }

  /* ---- state variables ---- */

  val tmodel = new TraceModelProxy(interpreter.newTraceModel)
  private var appState : PAppState = PStopped()

  /* ------ application logic --------- */

  def state: AppState =
    appState match {
      case PStopped() => Stopped()
      case PPaused(_, _) => Paused()
      case PInitialized(_, _) => Paused()
      case PPlaying(_, _, _) => Playing()
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

  private def setState(s: PAppState) = {
    val changed =
      (appState, s) match {
        case (PPlaying(_, _, _), PPlaying(_, _, _)) => false
        case (PStopped(), PStopped()) => false
        case (PPaused(_, _), PPaused(_, _)) => false
        case (PInitialized(_, _), PInitialized(_, _)) => false
        case _ => true
      }
    appState = s
    if (changed) Acumen.actor ! StateChanged()
  }
  private def emitProgress(p:Int) = Acumen.actor ! Progress(p)
  private def emitProgressMsg(msg:String) = Acumen.actor ! ProgressMsg(msg)
  private def emitError(e: Throwable) = { Acumen.actor ! Error(e); stop }
   
  private def withErrorReporting(action: => Unit) : Unit = {
    try action
    catch { case ce :ControlThrowable => throw ce
            case e => emitError(e) }
  }

  private def updateProgress(s: TraceData) = {
    val time = s.curTime
    val endTime = s.endTime
    emitProgress((time * 100 / endTime).toInt)
  }

  private def init: Unit = {
    tmodel.reset
    val ast = Parser.run(Parser.prog, text)
    // Disable for now, the enclosure semantics can't handle it
    //val dif = SD.run(ast)
    val des = Desugarer.run(ast)
    val I = interpreter
    val (prog, store) = I.init(des)

    if (isEnclosure) {
      setState(PInitialized(null, null))
    } else {
      val cstore = I.repr(store)
      //*********************************
      data.getData(cstore)
      tmodel.addData(CStoreTraceData(List(cstore))) // FIXME: What is this doing??? -kevina
      setState(PInitialized(prog, cstore))
    }

  }
  //******************************************************************************
  //***********************************************************************************

  def reset: Unit = {
    tmodel.reset
    setState(PStopped())
  }

  def play: Unit = withErrorReporting {

    def go(p: Prog, s: CStore) = {
      val co = new Consumer()
      val pr = new Producer(p, s, co)
      co.start
      pr.start

      setState(PPlaying(p, s, co))
    }
    appState match {
      case PStopped() =>
        init
        play
      case PInitialized(p, s) => go(p, s)
      case PPaused(p, s) => go(p, s)
      case _ => throw BadUiTransition("Can only play when Paused or Stopped.")
    }
  }

  def pause: Unit = {
    appState match {
      case PPlaying(p, s, c) => {
        setState(PPaused(p,
          (c !? Stop) match {
            case None => s
            case Some(ns) => ns.asInstanceOf[CStore]
          }))
      }
      case _ => throw BadUiTransition("Can only Pause while playing.")
    }
  }

  def step: Unit = withErrorReporting {
    def go(p: Prog, s: CStore) = {
      val I = interpreter
      val mstore = I.step(p, I.fromCStore(s)) map I.repr
      mstore match {
        case Some(st) =>
          val td = CStoreTraceData(List(st))
          tmodel.addData(td)
          setState(PPaused(p, st))
          updateProgress(td)
        case None =>
          setState(PStopped())
      }
    }
    appState match {
      case PPaused(p, s) => go(p, s)
      case PInitialized(p, s) => go(p, s)
      case PStopped() => { init; step }
      case _ => throw BadUiTransition("Step only allowed in Paused or Stopped mode.")
    }
  }

  def stop: Unit = {
    appState match {
      case PPlaying(_, _, p) => p ! Stop
      case _ => ()
    }
    setState(PStopped())
  }

  case object GoOn
  case object Stop
  case class Chunk(css: TraceData)
  case class Done(css: TraceData)

  private class Producer(p: Prog, st: CStore, consumer: Actor) extends Actor {
    var buffer = Queue.empty[CStore]
    def handle[A](code: => A) = {
      try { code }
      catch {
        case e =>
          consumer ! Error(e); exit
      }
    }

    def act : Unit = {
      if (isEnclosure) {
        // Bouncing Ball Example:
        // With buffer size = 12
        //   Time to run simulation: 32.566000
        //   Time to run simulation: 27.994000
        //   Time to run simulation: 28.202000
        // With buffer size = 1
        //   Time to run simulation: 33.578000
        //   Time to run simulation: 29.054000
        //   Time to run simulation: 28.657000
        val maxBufSize = 1
        var buf = new ArrayBuffer[UnivariateAffineEnclosure]
        val callbacks = new EnclosureInterpreterCallbacks {
          def log(msg: String) : Unit = {
            if (msg != null)
              Acumen.actor ! ProgressMsg(msg)
          }
          override def sendResult(d: Iterable[UnivariateAffineEnclosure]) {
            if (maxBufSize == 1) {
              consumer ! Chunk(new EnclosureTraceData(d, endTime))
            } else {
              buf ++= d
              if (buf.size > maxBufSize) {
                consumer ! Chunk(new EnclosureTraceData(buf, endTime))
                buf = new ArrayBuffer[UnivariateAffineEnclosure]
              }
            }
            receive {
              case GoOn => ()
              case Stop => exit
            }
          }
        }
        withErrorReporting {
          val s = System.currentTimeMillis
          interpreters.enclosure.Interpreter.runInterpreter(callbacks)
          consumer ! Done(new EnclosureTraceData(buf, 0))
          println("Time to run simulation: %f".format((System.currentTimeMillis - s)/1000.0))
        }
        exit
      }
      val I = interpreter
      val trace = handle { I.loop(p, I.fromCStore(st)) map I.repr }
      val iter = trace.iterator
      loopWhile(iter.hasNext) {
        if (buffer.size >= 200) {
          val buf = buffer
          consumer ! Chunk(CStoreTraceData(buf))
          react {
            case GoOn => buffer = Queue.empty[CStore]
            case Stop => exit
          }
        } else {
          handle { buffer = buffer enqueue iter.next }
        }
      } andThen {
        val buf = buffer
        consumer ! Done(CStoreTraceData(buf))
      }
    }
  }

  private class Consumer extends Actor {
    var last: Option[Object] = None
    var n = 1;
    def flush(css: TraceData) : Unit = {
      if (css.isEmpty)
        return
      val l = css.last
      Swing onEDT {
        tmodel addData css

        if (!isEnclosure) {
        //*****************************************
        //**************************************
        val _3DSampleInterval = 3;
        for (cs <- css.asInstanceOf[Iterable[CStore]]) {
          if (n > _3DSampleInterval) {
            data.getData(cs)
            n = 1;
          } else
            n += 1;
        }
        }
        updateProgress(css)
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
            //*******************************************

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

