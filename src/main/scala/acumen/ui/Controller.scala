package acumen
package ui

import Errors._

import scala.actors._
import scala.collection.JavaConversions._
import scala.swing._
import scala.swing.event._
import util.Canonical._
import util.Conversions._

import AppState._
import interpreter._
import interpreter.{InterpreterCntrl => IC}

case object SendInit

sealed abstract class AppEvent extends Event
case class StateChanged(st: AppState) extends AppEvent
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

  //*************************

  /* ---- state ---- */

  private var state : AppState = Stopped

  var model : InterpreterModel = null
  var threeDData = new threeD.ThreeDData;
  var producer : Actor = null

  /* ------ application logic --------- */

  private def setState(s: AppState) = {
    println("State Now: " + s)
    if (state != s) {
      state = s
      println("Acumen Actor State: " + Acumen.actor.getState)
      Acumen.actor ! StateChanged(state)
      if (state == Stopped) {
        unlink(producer)
        producer = null
      }
    }
  }

  private def updateProgress(s: TraceData) = {
    val time = s.curTime
    val endTime = s.endTime
    Acumen.actor ! Progress((time * 100 / endTime).toInt)
  }

  /* ------ actor logic ------------ */

  // The state to be in after receiving a chunk
  var newState : AppState = Stopped

  def act() {
    Supervisor.watch(this, "Controller")
    trapExit = true
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
            println("Acumen.actor state: " + Acumen.actor.getState)
            Acumen.actor ! SendInit
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
            Acumen.actor ! SendInit
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
        case IC.Chunk(d) =>
          println("Chunk")
          if (newState == Resuming) {println("...GoOn"); if (producer != null /* FIXME: Don't think this should happen*/) producer ! IC.GoOn}
          if (d != null) flush(d)
          setState(newState)
        case Exit(_,ue:UncaughtException) =>
          Acumen.actor ! Error(ue.cause)
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
    Swing.onEDT {
      println("Processing Data")
      model.addData(d)
      
      // FIXME: Do This here?
      // FIXME: be more precise ?
      if (seqNum == model.lastSeqNum) {
        val tm = model.getTraceModel
        //Swing.onEDTWait {
          Acumen.ui.traceTable.model = tm
          tm.fireTableStructureChanged()
        //}
        // FIMXE: Well you know ... (you = kevina)
        Acumen.ui.traceView.plotPanel.resetPlotModel(model.getPlotModel,Acumen.ui.traceView.check.selected)
        //tmodel.fireTableStructureChanged()
      } else {
        println("SKIPPING THIS ROUND!")
      }
      
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

