package acumen
package ui
package interpreter

import collection.immutable.Queue
import collection.mutable.ListBuffer
import scala.actors._
import InterpreterCntrl._
import java.io.File
import interpreters.Common.{deviceClass, paramModelTxt}

class CStoreCntrl(val semantics: SemanticsImpl[Interpreter], val interpreter: CStoreInterpreter) extends InterpreterCntrl {

  def newInterpreterModel = interpreter.newInterpreterModel

  def init(progText: String, currentDir: File, consumer:Actor) = new InterpreterActor(progText, consumer) {

    var buffer = Queue.empty[GStore]
    var defaultBufferSize = 200
    var bufferSize = 1 // start off with one step
    
    var timeOfLastFlush = 0L // milliseconds since epoch, ensures that plot is updated often enough
    val minPlotUpdateInterval = 100 // wait at most this many milliseconds before updating plot

    def parse() = {
      val ast = semantics.parse(progText, currentDir, None)
      val astWithPrelude = Prog(deviceClass :: ast.defs)
      val des = semantics.applyPasses(astWithPrelude, Main.extraPasses)
      prog = des
    }
    
    def sendChunk() {
      val toSend = if (buffer.isEmpty) null else CStoreTraceData(buffer)
      consumer ! Chunk(toSend)
      buffer = Queue.empty[GStore]
    }

    val emergencyActions : PartialFunction[Any,Unit] = {
      case Stop => sendChunk(); exit()
      case Flush => flush()
    }

    def flush() {
      sendChunk()
      timeOfLastFlush = System.currentTimeMillis
      react (emergencyActions orElse {
        case GoOn => bufferSize = defaultBufferSize
        case Step => bufferSize = 1
        case msg => println("Unknown msg received by producer: " + msg)
      })
    }

    // CStoreFilterDataAdder for Interpreters 2014 or before
    class LegacyCStoreFilterDataAdder(opts: CStoreOpts) extends CStoreFilterDataAdder(opts) {
      override val initialShouldAddData = ShouldAddData.IfLast
      override def addLast : Boolean = (shouldAddData == ShouldAddData.IfLast)
      prevStepType = Discrete
      curStepType = Discrete
      shouldAddData = ShouldAddData.IfLast
    } 
    
    // CStoreFilterDataAdder - uses the "buffer" of the actor "init"
    class CStoreFilterDataAdder(opts : CStoreOpts) extends FilterDataAdder(opts) {
        outputRow = true
        var buffer2 = new ListBuffer[(CId,GObject)]
        override def noMoreData() = {
          super.noMoreData()
          if (buffer2.nonEmpty)
            buffer = buffer enqueue buffer2
        }
        def addData(objId: CId, values: GObject) = {
          val plotFilter = getPlotFilter(values)
          buffer2 += (objId -> values.toList.filter(mkFilter(values, plotFilter)))
        }
        def continue = {
          if (outputRow) {
            buffer = buffer enqueue buffer2
            buffer2 = new ListBuffer[(CId,GObject)]
          }
          //buffer.size < bufferSize 
          false
        }
    }
    
    def produce : Unit = {
      val startTime = System.currentTimeMillis
      val I = interpreter
      val (p, store0, md0) = I.init(prog)
      var opts = new CStoreOpts
      // Variables for real-time simulation
      val threeDTab = App.ui.threeDtab.asInstanceOf[threeD.ThreeDTab]
      var lastvirtualTime = 0.0
      var timesteps = 0
      var slackvalue = 0.0
      var updateTime = 0.0
      var missedDeadline = 0.0
      // Create the DataAdder
      val adder = if (semantics.isOldSemantics) {opts.outputRows = OutputRows.WhenChanged; new LegacyCStoreFilterDataAdder(opts)} 
                  else                                                                     new CStoreFilterDataAdder(opts)
      // Add initial store to trace
      I.repr(store0).foreach{case (id,v) => adder.addData(id, v)}
      adder.continue
      // Read simulator parameters from program
      var (store, md, endTime) = I.multiStep(p, store0, md0, adder)
      val cstore = I.repr(store)
      acumen.util.Canonical.getInSimulator(Name("outputRows",0), cstore) match {
        case VLit(GStr("All"))              => opts.outputRows = OutputRows.All
        case VLit(GStr("WhenChanged"))      => opts.outputRows = OutputRows.WhenChanged
        case VLit(GStr("FinalWhenChanged")) => opts.outputRows = OutputRows.FinalWhenChanged
        case VLit(GStr("ContinuousOnly"))   => opts.outputRows = OutputRows.ContinuousOnly
        case VLit(GStr("Last"))             => opts.outputRows = OutputRows.Last
        case _                              => /* fixme: throw error */
      }
      acumen.util.Canonical.getInSimulator(Name("continuousSkip",0), cstore) match {
        case VLit(GInt(n)) => opts.continuousSkip = n
        case _             => /* fixme: throw error */
      }
      acumen.util.Canonical.getInSimulator(Name("hypothesisReport",0), cstore) match {
        case VLit(GStr("Ignore"))            => md = NoMetadata(Some(HypothesisResultFilter.Ignore), None) combine md
        case VLit(GStr("Comprehensive"))     => md = NoMetadata(Some(HypothesisResultFilter.Comprehensive), None) combine md
        case VLit(GStr("IgnoreInitialOnly")) => md = NoMetadata(Some(HypothesisResultFilter.IgnoreInitialOnly), None) combine md
        case VLit(GStr("MostSignificant"))   => md = NoMetadata(Some(HypothesisResultFilter.MostSignificant), None) combine md
        case _                               => /* fixme: throw error */
      }
      loopWhile(!adder.done) {
        reactWithin(0) (emergencyActions orElse {
          case TIMEOUT =>
            val temptime = System.currentTimeMillis
            val (store1, md1, endTime1) = I.multiStep(p, store, md, adder)
            store = store1
            md = md1
            endTime = endTime1

            threeDTab.appModel.threeDData.get3DData(I.repr(store))
            threeDTab.appModel.updateProgress(I.repr(store))

            if (threeDTab.checkRTAnimation.selected) {
              // render the latest frame
              if (threeDTab.appModel.threeDData._3DData.nonEmpty) {
                threeDTab.playinRealTime()
              }
              // calculate the real time performance
              val virtualtime = acumen.util.Canonical.getTime(I.repr(store))
              val playspeed = threeDTab.playSpeed
              var averageSlack = 0.0
              val calculationTime = System.currentTimeMillis - temptime

              // set slider bar
              val simulationEndTime = acumen.util.Canonical.getEndTime(I.repr(store))
              val slidePercentage = (virtualtime * 100 / simulationEndTime).toInt
              if (slidePercentage >= 100)
                threeDTab.statusZone3d.setProgress3D(100)
              else
                threeDTab.statusZone3d.setProgress3D(slidePercentage)

              if (calculationTime > (virtualtime - lastvirtualTime) * 1000 / playspeed
                && virtualtime > lastvirtualTime)
                missedDeadline += 1
              timesteps += 1
              val percentagemissDL = missedDeadline / timesteps
              // for synchronizing the simulation with wall clock
              if (threeDTab.checkMatchTime.selected){
                // calculate the averageslack
                if (virtualtime > lastvirtualTime) {
                  if ((virtualtime - lastvirtualTime) * 1000 / playspeed < calculationTime)
                    slackvalue = 0 + slackvalue
                  else
                    slackvalue = ((virtualtime - lastvirtualTime) * 1000 /
                      playspeed) - (System.currentTimeMillis - temptime) +
                      slackvalue
                }
                averageSlack = slackvalue / (System.currentTimeMillis - startTime)
                realtimeTimer(virtualtime, playspeed, startTime)
              } else
                averageSlack = 0
              if ((virtualtime - updateTime) * 1000 > 100) {
                // update every 100ms
                threeDTab.threeDView.percentagemissDL = percentagemissDL
                threeDTab.threeDView.averageSlack = averageSlack
                if (threeDTab.checkMatchTime.selected) {
                  threeDTab.missedDeadLine.text = "   Missed deadlines:%.2f".format(percentagemissDL * 100) + "%    "
                  threeDTab.slackTime.text = "Slack:%.2f".format(averageSlack * 100) + "%  "
                }
                updateTime = virtualtime
              }
              lastvirtualTime = virtualtime
            }
            
            if (buffer.size >= bufferSize || (System.currentTimeMillis -
              timeOfLastFlush) > minPlotUpdateInterval) flush()
            
        })
      } andThen {
        sendChunk()
        App.ui.stopSimulation()
        consumer ! Done(List("Time to run simulation: %.3fs".format((System.currentTimeMillis - startTime) / 1000.0)), md, endTime)
      }
    }
  }

  def realtimeTimer(virtualtime: Double, playSpeed: Double, startTime: Double) = {
    var sleeptime = 0.0
    var extratime = 0.0
    if (System.currentTimeMillis - startTime > virtualtime * 1000 / playSpeed){
      // do nothing
      sleeptime = 0.0
      extratime = 0.0
    }else {
      sleeptime = virtualtime * 1000 / playSpeed - (System.currentTimeMillis - startTime)
      extratime = (sleeptime - sleeptime.toLong) * 1000000 // To nano sec
      Thread.sleep(sleeptime.toLong, extratime.toInt)
    }
  }
}
