package acumen
package ui
package interpreter

import collection.immutable.Queue
import collection.mutable.ListBuffer
import scala.actors._
import InterpreterCntrl._
import java.io.File
import interpreters.Common.{deviceClass, paramModelTxt,paraClass}

class CStoreCntrl(val semantics: SemanticsImpl[Interpreter], val interpreter: CStoreInterpreter) extends InterpreterCntrl {

  def newInterpreterModel = interpreter.newInterpreterModel

  def init(progText: String, currentDir: File, consumer:Actor) = new InterpreterActor(progText, consumer) {

    var buffers = collection.mutable.Map.empty[Tag, Queue[GStore]]
    val deadTags = collection.mutable.Set.empty[Tag]
    var defaultBufferSize = 200
    var bufferSize = 1 // start off with one step
    
    var timeOfLastFlush = 0L // milliseconds since epoch, ensures that plot is updated often enough
    val minPlotUpdateInterval = 100 // wait at most this many milliseconds before updating plot

    def parse() = {
      val ast = semantics.parse(progText, currentDir, None)
      val astWithPrelude = Prog(deviceClass :: paraClass :: ast.defs)
      val des = semantics.applyPasses(astWithPrelude, Main.extraPasses)
      prog = des
    }
    
    def sendChunk() {
      val toSend = buffers mapValues (buf => if (buf.isEmpty) null else CStoreTraceData(buf))
      toSend foreach { case (tag, td) => consumer ! Chunk((tag, deadTags.contains(tag)), td) }
      buffers.clear()
      deadTags.clear()
    }

    val emergencyActions : PartialFunction[Any,Unit] = {
      case Stop => sendChunk(); exit()
      case Flush => flush()
    }

    def flush() {
      timeOfLastFlush = System.currentTimeMillis
      if (buffers.nonEmpty) {
        sendChunk()
        react(emergencyActions orElse {
          case GoOn => bufferSize = defaultBufferSize
          case Step => bufferSize = 1
          case msg => println("Unknown msg received by producer: " + msg)
        })
      }
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
    class CStoreFilterDataAdder(opts: CStoreOpts) extends FilterDataAdder(opts) {
      outputRow = true
      var buffer2 = collection.mutable.Map.empty[Tag, ListBuffer[(CId, GObject)]]

      override def noMoreData() = {
        super.noMoreData()
        buffer2 foreach { case (tag, buf) =>
          if (buf.nonEmpty)
            if (buffers.contains(tag))
              buffers(tag) enqueue buf
            else
              buffers += tag -> (Queue.empty[GStore] enqueue buf)
        }
      }

      def addData(objId: CId, values: GObject, tag: Tag = Tag.root, deadTag: Boolean = false) = {
        val plotFilter = getPlotFilter(values)
        if (buffer2.contains(tag))
          buffer2(tag) += (objId -> values.toList.filter(mkFilter(values, plotFilter)))
        else
          buffer2 += tag -> (ListBuffer.empty += (objId -> values.toList.filter(mkFilter(values, plotFilter))))
        if (deadTag) deadTags += tag
      }

      def continue = {
        if (outputRow) {
          buffer2 foreach { case (tag, buf) =>
            if (buf.nonEmpty)
              if (buffers.contains(tag))
                buffers(tag) enqueue buf
              else
                buffers += tag -> (Queue.empty enqueue buf)
            buffers(tag) = buffers(tag) enqueue buf
          }
          buffer2.clear()
        }
        false
      }
    }
    
    def produce: Unit = {
      val startTime = System.currentTimeMillis
      val I = interpreter
      //Three types to store respectively the result of a multiStep call, the options and the adder for each split
      type multiStepResMap = collection.mutable.Map[Tag, (I.Store, Metadata, Double)]
      type optionsMap = collection.mutable.Map[Tag, CStoreOpts]
      type addersMap = collection.mutable.Map[Tag, DataAdder]

      //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      //Initialisation of the simulation--------------------------------------------------------------------------------
      val (p, sstore0, mds0) = I.init(prog)
      //Used by the 3D view to refer to one arbitrary Store and stock to it (repeated used of .head instead on a mutable map is not safe)
      val (headTag, _) = sstore0.head
      val opts: optionsMap = collection.mutable.Map() ++ (sstore0 map { case (tag, _) => tag -> new CStoreOpts })
      // Variables for real-time simulation
      val threeDTab = App.ui.threeDtab.asInstanceOf[threeD.ThreeDTab]
      var lastvirtualTime = 0.0
      var timesteps = 0
      var slackvalue = 0.0
      var updateTime = 0.0
      var missedDeadline = 0.0
      var biggestTime = 0.0
      // Create the DataAdders
      val adders: addersMap = collection.mutable.Map() ++
        (if (semantics.isOldSemantics) {
          opts mapValues (opt => {
            opt.outputRows = OutputRows.WhenChanged
            new LegacyCStoreFilterDataAdder(opt)
          })
        } else
          opts mapValues (opt => new CStoreFilterDataAdder(opt)))
      // Add initial SuperStore to trace
      I.addData(sstore0, adders)
      adders.values.foreach(_.continue)

      val multiStepRes: multiStepResMap = collection.mutable.Map.empty ++
        (sstore0 map { case (tag, st) => tag -> (st, mds0(tag), 0.0) })
      performMultiStep()

      // Read simulator parameters from program
      multiStepRes foreach { case (tag, (st, md, endTime)) =>
        val cstore = I.repr(st)
        var newMd = md
        acumen.util.Canonical.getInSimulator(Name("outputRows", 0), cstore) match {
          case VLit(GStr("All")) => opts(tag).outputRows = OutputRows.All
          case VLit(GStr("WhenChanged")) => opts(tag).outputRows = OutputRows.WhenChanged
          case VLit(GStr("FinalWhenChanged")) => opts(tag).outputRows = OutputRows.FinalWhenChanged
          case VLit(GStr("ContinuousOnly")) => opts(tag).outputRows = OutputRows.ContinuousOnly
          case VLit(GStr("Last")) => opts(tag).outputRows = OutputRows.Last
          case _ => /* fixme: throw error */
        }
        acumen.util.Canonical.getInSimulator(Name("continuousSkip", 0), cstore) match {
          case VLit(GInt(n)) => opts(tag).continuousSkip = n
          case _ => /* fixme: throw error */
        }
        acumen.util.Canonical.getInSimulator(Name("hypothesisReport", 0), cstore) match {
          case VLit(GStr("Ignore")) => newMd = NoMetadata(Some(HypothesisResultFilter.Ignore), None) combine md
          case VLit(GStr("Comprehensive")) => newMd = NoMetadata(Some(HypothesisResultFilter.Comprehensive), None) combine md
          case VLit(GStr("IgnoreInitialOnly")) => newMd = NoMetadata(Some(HypothesisResultFilter.IgnoreInitialOnly), None) combine md
          case VLit(GStr("MostSignificant")) => newMd = NoMetadata(Some(HypothesisResultFilter.MostSignificant), None) combine md
          case _ => /* fixme: throw error */
        }
        multiStepRes(tag) = (st, newMd, endTime)
      }
      //End of the initialisation---------------------------------------------------------------------------------------
      //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

      loopWhile(multiStepRes.nonEmpty && !adders.values.forall(_.done)) {
        reactWithin(0)(emergencyActions orElse {
          //Several GoOn are received after each flush and only one is consumed buy flush(). The remaining one are consumed here.
          //If they are not consumed on time, they will accumulate and block the pausing process
          //Another solution would be to have a criterion to return GoOn from the consumer but I don't think that it is a safe method.
          case GoOn => bufferSize = defaultBufferSize
          case TIMEOUT =>
            val temptime = System.currentTimeMillis
            // Perform a multistep
            performMultiStep()
            // Update the progress bar
            //FIXME: Not a fully satisfying way to update the progressBar: The first Store might end before the others or never end.
            //Indeed, there is no synchronisation guarantee between the stores (e.g. with adaptive stepping)
            if (multiStepRes.nonEmpty) threeDTab.appModel.updateProgress(I.repr(multiStepRes.head._2._1))

            //The 3D view is activated with the first Store arbitrarily.
            //If there is no split, then it is the classic behaviour, if there is, then it might lead to funny things.
            if (multiStepRes.contains(headTag)) {
              val headStore = multiStepRes(headTag)._1
              threeDTab.appModel.threeDData.get3DData(I.repr(headStore))

              if (threeDTab.checkRTAnimation.selected) {
                // render the latest frame
                if (threeDTab.appModel.threeDData._3DData.nonEmpty) {
                  threeDTab.playinRealTime()
                }
                // calculate the real time performance
                val virtualtime = acumen.util.Canonical.getTime(I.repr(headStore))
                val playspeed = threeDTab.playSpeed
                var averageSlack = 0.0
                val calculationTime = System.currentTimeMillis - temptime

                // set slider bar
                val simulationEndTime = acumen.util.Canonical.getEndTime(I.repr(headStore))
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
                if (threeDTab.checkMatchTime.selected) {
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
            }
            if (buffers.values.map(_.size).forall(bufferSize <) ||
              (System.currentTimeMillis - timeOfLastFlush) > minPlotUpdateInterval)
              flush()

        })
      } andThen {
        sendChunk()
        App.ui.stopSimulation()
        //TODO: For now, the final MetaData is a simple combination of the previous one but it should be upgraded
        val (md, endTime) = if (multiStepRes.nonEmpty)
          (multiStepRes.values.map(_._2) reduce ((l, r) => l combine r),
            biggestTime)
        else
          (NoMetadata, biggestTime)
        consumer ! Done(List("Time to run simulation: %.3fs".format((System.currentTimeMillis - startTime) / 1000.0)), md, endTime)
      }

      def performMultiStep() = {
        //res contains the results of multiStep on each Store
        val res = multiStepRes flatMap {
          case (tag, (store, md, endTime)) =>
            if (!adders(tag).done)
              Some(tag -> I.multiStep(p, store, md, adders(tag), tag))
            else None
        }
        //If a split occurred, then the result contains several Stores with the new tags
        //tag is the tagS of the Store sent to multiStep, tagR is one of the returned tags
        res foreach { case (tagS, msr) =>
          val splitOccurred = msr.size > 1
          var deadStores = Set.empty[Tag]
          msr foreach { case r@(tagR, (st, md, et)) =>
            if (I.isDead(st)) deadStores += tagR
            else {
              multiStepRes += r
              if (splitOccurred) {
                //The options and the adders are set once and for all at the initialisation.
                opts += (tagR -> opts(tagS))
                adders += (tagR -> (if (semantics.isOldSemantics) new LegacyCStoreFilterDataAdder(opts(tagR)) else new CStoreFilterDataAdder(opts(tagR))))
                if (adders(tagS).done) adders(tagR).noMoreData() //If the previous adder is Done, then the new adders must be done too
              }
            }
          }
          //Set the new biggest time before deleting some stores
          biggestTime = multiStepRes.values.map(r => {
            val (_, _, endTime) = r;
            endTime
          }).max
          //The Stores which have been split or which died do not need to exist anymore
          if (splitOccurred || deadStores.nonEmpty) {
            val toRemove = deadStores + tagS
            multiStepRes --= toRemove
            opts --= toRemove
            adders --= toRemove
          }
        }
      }
    }
  }

  def realtimeTimer(virtualtime: Double, playSpeed: Double, startTime: Double) = {
    var sleeptime = 0.0
    var extratime = 0.0
    if (System.currentTimeMillis - startTime > virtualtime * 1000 / playSpeed) {
      // do nothing
      sleeptime = 0.0
      extratime = 0.0
    } else {
      sleeptime = virtualtime * 1000 / playSpeed - (System.currentTimeMillis - startTime)
      extratime = (sleeptime - sleeptime.toLong) * 1000000 // To nano sec
      Thread.sleep(sleeptime.toLong, extratime.toInt)
    }
  }

}
