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
import InterpreterModel._

case object SendInit

sealed abstract class AppEvent extends Event
case class StateChanged(st: AppState.State) extends AppEvent
case class Error(e: Throwable) extends AppEvent
case class Progress(percent: Int) extends AppEvent
case class ProgressMsg(msg:String) extends AppEvent
case class Progress3d(percent: Int) extends AppEvent

sealed abstract class AppActions
case class Init(t:String, i:InterpreterModel) extends AppActions
case object Play extends AppActions
case object Stop extends AppActions
case object Pause extends AppActions
case object Step extends AppActions

class Controller extends DaemonActor {

  //*************************

  /* ---- state ---- */

  private var state : State = Stopped

  val tmodel = new TraceModel(FakeTraceModelData)
  var threeDData = new threeD.ThreeDData;
  var producer : Actor = null

  /* ------ application logic --------- */

  private def setState(s: State) = {
    println("State Now: " + s)
    if (state != s) {
      state = s
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
  var newState : State = Stopped

  def act() {
    trapExit = true
    loop {
      react {
        //
        // messages from UI
        //
        case Init(progText, interpreter) => 
          println("INIT")
          tmodel.data = interpreter.newTraceModelData
          producer = interpreter.init(progText, this)
          link(producer)
          producer.start()
          setState(Playing)
        case Play  =>
          newState = Playing
          println("PLAY")
          if (producer == null) {
            Acumen.actor ! SendInit
          } else {
            producer ! GoOn
            setState(Playing)
          }
        case Pause => 
          println("PAUSE")
          if (producer != null)
            producer ! Flush
          newState = Paused
          println("^^^PAUSE^^^\n")
        case Step  => 
          println("STEP")
          newState = Paused
          if (producer == null) {
            Acumen.actor ! SendInit
          } else {
            producer ! Step
            setState(Playing)
          }
        case Stop  => 
          println("STOP")
          if (producer == null) {
            setState(Stopped)
          } else {
            println("Sending Stop")
            producer ! Stop
            newState = Stopped
          }
        //
        // messages from Producer
        //
        case Chunk(d) =>
          println("Chunk")
          if (newState == Playing) {println("...GoOn"); producer ! GoOn}
          if (d != null) flush(d)
          setState(newState)
        case Exit(_,ue:UncaughtException) =>
          Acumen.actor ! Error(ue.cause)
          setState(Stopped)
        case msg@(Done | Exit(_,_)) => 
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
    val seqNum = tmodel.incSeqNum()
    Swing.onEDT {
      println("Processing Data")
      tmodel.addData(d, seqNum)
      
      // FIXME: Do This here?
      // FIXME: be more precise ?
      if (seqNum == tmodel.lastSeqNum)  
        tmodel.fireTableStructureChanged()
      else 
        println("SKIPPING THIS ROUND!")
      
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

