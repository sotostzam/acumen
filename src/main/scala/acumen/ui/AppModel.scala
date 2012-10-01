package acumen
package ui

import Errors._

import util.Canonical._
import util.Conversions._

import java.lang.Thread
import collection.JavaConversions._
import collection.immutable.Queue
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import java.io._
import swing._
import swing.event._
import scala.actors._

sealed abstract class AppEvent extends Event
case class InterpreterChanged()  extends AppEvent
case class StateChanged() extends AppEvent
case class Error(e: Throwable) extends AppEvent
case class Progress(percent: Int) extends AppEvent
case class ProgressMsg(msg:String) extends AppEvent
case class Progress3d(percent: Int) extends AppEvent
case class Playing3d() extends AppEvent

sealed abstract class AppState
case class Stopped() extends AppState
case class Playing() extends AppState
case class Paused() extends AppState

sealed abstract class InterpreterType
case class Pure() extends InterpreterType
case class Impure(nbThreads: Int) extends InterpreterType

class AppModel(text: => String, console: Console) extends Publisher {

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

    publish(InterpreterChanged())
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
    if (changed) publish(StateChanged())
  }
  private def emitProgress(p:Int) = publish(Progress(p))
  private def emitProgressMsg(msg:String) = publish(ProgressMsg(msg))
  private def emitError(e: Throwable) = { publish(Error(e)); stop }
   
  private def withErrorReporting(action: => Unit) : Unit = {
    try action
    catch { case e => emitError(e) }
  }

  private def updateProgress(s: TraceData) = {
    val time = s.curTime
    val endTime = s.endTime
    emitProgress((time * 100 / endTime).toInt)
  }

  private def init: Unit = {
    tmodel.reset
    if (isEnclosure) {
      setState(PInitialized(null, null))
    } else {
      val ast = Parser.run(Parser.prog, text)
      val dif = SD.run(ast)
      val des = Desugarer.run(dif)
      val I = interpreter
      val (prog, store) = I.init(des)

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
  case class Message(msg: String)
  case class Chunk(css: TraceData)
  case class Done(css: TraceData)
  case class EnclosureDone(tm: AbstractTraceModel)

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
        def log(msg: String) : Unit = {
          if (msg != null)
            consumer ! Message(msg)
          receive {
            case GoOn => ()
            case Stop => throw new java.lang.InterruptedException
          }
        }
        // FIXME: Is it okay to use withErrorReporting here in the producer
        withErrorReporting {
          try {
            val tm = interpreters.enclosure.Interpreter.generateTraceModel(text,log)
            consumer ! EnclosureDone(tm)
          } catch {
            case _:java.lang.InterruptedException => exit
          }
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
    def flush(css: TraceData) = {
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

    def waitForResult {
      react {
        case EnclosureDone(tm) =>
          tmodel.setTraceModel(tm)
          tmodel.fireTableStructureChanged()
          exit
        }
    }

    def finish = {
      react {
        case Done(_) | Chunk(_) | Message(_) => 
          reply(Stop)
          if (isEnclosure)
            waitForResult
          else
            exit
      }
    }

    def act = {
      loop {
        react {
          case Stop =>
            reply(last)
            finish
          case Message(msg) =>
            reply(GoOn)
            emitProgressMsg(msg)
          case Chunk(css) =>
            reply(GoOn)
            flush(css)
          case Done(css) =>
            flush(css)
            //*******************************************

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

/* Get the 3D-visualization data */
class ThreeDData(console: Console) extends Publisher {
  /* Stores all the information for 3D-visualization */
  type _3DStore = Map[CId, _3DClass];
  /* Stores 3D-visualization information for a class */
  type _3DClass = Map[Int, List[List[_]]]
  var _3DData: _3DStore = Map[CId, _3DClass]()
  /* The number of 3D-objects */
  var objectCount = 1;
  var frameNumber = 0
  /* Used for determine 3D-visualization play speed */
  var endTime = 0.0;
  /* Default settings to transform Acumen AST to generic data can be used later for Java3D
       Example : GStr("Sphere") => "Sphere" 	                                            */
  var _3DType = "Sphere"
  var _3DPosition = Array[Double](0.0, 0.0, 0.0)
  var _3DSize = Array[Double]();
  var _3DColor = Array[Double](1.0, 1.0, 1.0)
  var _3DAngle = Array[Double](0.0, 0.0, 0.0)

  def reset() {
    _3DData.clear
    frameNumber = 0;
  }
  def init3DClassStore(id: CId, _3DData: _3DStore, objectCount: Int): Unit = {
    var temp: _3DClass = Map[Int, List[List[_]]]();
    for (i <- 0 to objectCount - 1) {
      temp += i -> List[List[_]]()
    }
    _3DData += id -> temp
  }
  def isVectorOfNumbers(l: List[_]): Boolean = {
    var result = true;
    for (x <- l)
      x match {
        case VLit(GInt(i)) =>
        case VLit(GDouble(i)) =>
        case _ => result = false;
      }
    result
  }
  /* _3DType should be a String or a Integer */
  def extractType(value: Value[_]) {
    value match {
      case VLit(GStr(s)) => this._3DType = s;
      case VLit(GInt(i)) => this._3DType = i.toString;
      case _ => throw _3DNameError(value)
    }
  }
  /* Check if the list's size is 3 and only contains numbers (int, double) */
  def checkVectorContent(l: List[_]): Boolean = {
    (l.size == 3) && isVectorOfNumbers(l)
  }
  /* _3D Position,color,angle should all be a vector with 3 numbers */
  def extractVector(value: Value[_], index: String) {
    var temp = Array[Double](0.0, 0.0, 0.0)
    value match {
      case VVector(vs) => {
        if (checkVectorContent(vs))
          temp = extractDoubles(vs).toArray
        else throw _3DVectorError(value, index);
      }
      case _ => throw _3DVectorError(value, index);
    }
    index match {
      case "position" => _3DPosition = temp
      case "color" => _3DColor = temp
      case "angle" => _3DAngle = temp
      case _ => throw ShouldNeverHappen()
    }
  }
  /* _3D size should be either a vector or an number */
  def extractSize(value: Value[_]) {
    value match {
      case VVector(vs) => {
        if (isVectorOfNumbers(vs)) _3DSize = extractDoubles(vs).toArray;
        else { _3DSize = Array[Double](); throw _3DSizeError(value) }
      }
      case VLit(GInt(x)) => _3DSize = Array(x.toDouble)
      case VLit(GDouble(x)) => _3DSize = Array(x)
      case _ => throw _3DSizeError(value)
    }
    /* Check if the _3DSize is valid */
    this._3DType match {
      case "Sphere" => if (_3DSize.length != 1) throw _3DSphereSizeError()
      case "Cylinder" => if (_3DSize.length != 2) throw _3DCylinderSizeError()
      case "Cone" => if (_3DSize.length != 2) throw _3DConeSizeError()
      case "Box" => if (_3DSize.length != 3) throw _3DBoxSizeError()
      /* 3D text's size should be a number */
      case _ => if (_3DSize.length != 1) throw _3DTextSizeError()
    }
  }

  /* Add new information of each 3D-object to _3DStore */
  def addTo3DStore(id: CId, _3DData: _3DStore, value: List[Value[_]], objectCount: Int) {
    if (!_3DData.contains(id))
      init3DClassStore(id, _3DData, objectCount);

    for (i: Int <- 0 to objectCount - 1) {
      var vector = value(i);
      vector match {
        case VVector(l) => {
          if (l.size != 5)
            throw _3DError(vector)
          else {
            extractType(l(0))
            extractSize(l(2))
            extractVector(l(1), "position")
            extractVector(l(3), "color")
            extractVector(l(4), "angle")
          }
        }
        case _ => throw ShouldNeverHappen()
      }
      _3DData(id)(i) = List(_3DType, _3DPosition,
        _3DSize, _3DColor, _3DAngle, frameNumber) :: _3DData(id)(i)
    }
  }
  /* Look for endTime in "Main" class */
  def lookUpEndTime(id: CId, o: CObject) {
    if (id.equals(new CId(List(0)))) {
      for ((name, value) <- o.toList) {
        if (name.x == "endTime") {
          this.endTime = extractDouble(value)
        }
      }
    }
  }
  /* Add _3D information of every class to _3DStore */
  def getData(s: CStore) {
    for ((id, o) <- s.toList) {
      lookUpEndTime(id, o);
      /* Look for variable named _3D */
      for ((name, value) <- o.toList) {
        if (name.x == "_3D") {
          value match {
            case VVector(l) => {
              if (l.size == 0)
                throw _3DError(value)
              else
                l(0) match {
                  /* If it's only one object, _3D will start with a string or an int, 
								*		example:  _3D = ["Sphere",...,...,..] 
								*	    		  	_3D = [2,...,...,..];  
								*/
                  case VLit(_) => addTo3DStore(id, _3DData, List(value), 1)
                  /**
                   * If it contains multiple objects, _3D will start with a vector,
                   * 		example : _3D = [["Sphere",[],[]...]
                   *                    ["Sphere",[],[]...]..]
                   */
                  case VVector(some) => addTo3DStore(id, _3DData, l, l.size)
                  case _ => throw _3DError(value)
                }
            }
            case _ => throw _3DError(value)
          }
        }
      }
    }
    frameNumber += 1
  }
}
