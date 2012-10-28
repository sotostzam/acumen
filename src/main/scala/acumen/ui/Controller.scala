package acumen
package ui

import Errors._

import scala.actors._
import scala.collection.JavaConversions._
import scala.swing._
import scala.swing.event._
import util.Canonical._
import util.Conversions._

import interpreter._
import interpreter.{InterpreterCntrl => IC}

case object SendInit

sealed abstract class AppEvent
case class Error(e: Throwable) extends AppEvent
case class Progress(percent: Int) extends AppEvent
case class ProgressMsg(msg:String) extends AppEvent
case class Progress3d(percent: Int) extends AppEvent

sealed abstract class AppActions
case class Init(t:String, i:InterpreterCntrl) extends AppActions
case object Play extends AppActions
case object Stop extends AppActions
case object Pause extends AppActions
case object Step extends AppActions

class Controller extends DaemonActor {
  import App._

  //*************************

  /* ---- state ---- */

  private var state : State = Stopped

  var model : InterpreterModel = null
  var threeDData = new threeD.ThreeDData;
  var producer : Actor = null

  /* ------ application logic --------- */

  private def setState(s: State) = {
    println("State Now: " + s)
    if (state != s) {
      state = s
      println("Acumen Actor State: " + actor.getState)
      actor ! state
      if (state == Stopped && producer != null) {
        unlink(producer)
        producer = null
      }
    }
  }

  private def updateProgress(s: TraceData) = {
    val time = s.curTime
    val endTime = s.endTime
    actor ! Progress((time * 100 / endTime).toInt)
  }

  /* ------ actor logic ------------ */

  // The state to be in after receiving a chunk
  var newState : State = Stopped

  def act() {
    Supervisor.watch(this, "Controller", {restart})
    trapExit = true
    setState(Stopped)
    loop {
      react {
        //
        // messages from UI
        //
        case Init(progText, interpreter) => 
          println("INIT")
          model = interpreter.newInterpreterModel
          producer = interpreter.init(progText, this)
          link(producer)
          producer.start()
          setState(Starting)
        case Play  =>
          newState = Resuming
          println("PLAY")
          if (producer == null) {
            println("No Producer Requesting Init")
            println("Acumen.actor state: " + actor.getState)
            actor ! SendInit
          } else {
            println("Producer State: " + producer.getState)
            producer ! IC.GoOn
            setState(Resuming)
          }
        case Pause => 
          println("PAUSE")
          if (producer != null)
            producer ! IC.Flush
          newState = Paused
          println("^^^PAUSE^^^\n")
        case Step  => 
          println("STEP")
          newState = Paused
          if (producer == null) {
            actor ! SendInit
          } else {
            producer ! IC.Step
            setState(Resuming)
          }
        case Stop  => 
          println("STOP")
          if (producer == null) {
            println("No producer.")
            setState(Stopped)
          } else {
            println("Sending Stop")
            println("Producer State: " + producer.getState)
            producer ! IC.Stop
            newState = Stopped
          }
        //
        // messages from Producer
        //
        case IC.Chunk(d) if sender == producer =>
          if (newState == Resuming) producer ! IC.GoOn
          if (d != null) flush(d)
          setState(newState)
        case IC.Chunk(d) => // ignore chunks from supposedly dead producers
        case Exit(_,ue:UncaughtException) =>
          actor ! Error(ue.cause)
          setState(Stopped)
        case msg@(IC.Done | Exit(_,_)) => 
          println("Done|Exit: " + msg)
          setState(Stopped)
        
        //
        //
        //
        case msg =>
          println("Unknown Message Received by Controller: " + msg)
      }
    }
  }

  // FIXME: The CStore Interpreter init gives back an initial CStore
  //  what is the purpose of it, and in the refactoring from AppModel
  //  am I still handling the saturation correctly, especially with
  //  the 3D code
  var n = 0
  def flush(d: TraceData) : Unit = {
    if (d.isEmpty)
      return
    val seqNum = model.incSeqNum()
    model.addData(d)
    // FIXME: This is still sick
    App.ui.traceView.plotPanel.plotter ! plot.Refresh
      
    Swing.onEDT {
      // d.isInstanceOf[Iterable[CStore]] will not work due to type
      // erasure, must check the first element for its type
      if (!d.isEmpty() && d.head.isInstanceOf[CStore]) {
        val _3DSampleInterval = 3;
        for (cs <- d.asInstanceOf[Iterable[CStore]]) {
          if (n == 0 || n > _3DSampleInterval) {
            threeDData.getData(cs)
            n = 1;
          } else
            n += 1;
        }
      }
      
      updateProgress(d)
    }
  }
}

