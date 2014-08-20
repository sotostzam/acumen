package acumen
package ui
package interpreter

import collection.immutable.Queue
import collection.mutable.ListBuffer
import scala.actors._
import InterpreterCntrl._
import java.io.File

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
      val des = semantics.applyPasses(ast, Main.extraPasses)
      prog = des
    }
    
    def sendChunk {
      val toSend = if (buffer.isEmpty) null else CStoreTraceData(buffer)
      consumer ! Chunk(toSend)
      buffer = Queue.empty[GStore]
    }

    val emergencyActions : PartialFunction[Any,Unit] = {
      case Stop => { sendChunk; exit }
      case Flush => flush
    }

    def flush {
      sendChunk
      timeOfLastFlush = System.currentTimeMillis
      react (emergencyActions orElse {
        case GoOn => bufferSize = defaultBufferSize
        case Step => bufferSize = 1
        case msg => println("Unknown msg received by producer: " + msg)
      })
    }

    def produce : Unit = {
      val startTime = System.currentTimeMillis
      val I = interpreter
      val (p, store0, md0) = I.init(prog)
      var (store, md) = I.multiStep(p, store0, md0, new StopAtFixedPoint)
      val cstore = I.repr(store)
      var opts = new CStoreOpts
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
      val adder = new FilterDataAdder(opts) {
        var buffer2 = new ListBuffer[(CId,GObject)]
        override def noMoreData() = {
          super.noMoreData()
          if (buffer2.nonEmpty)
            buffer = buffer enqueue buffer2
        }
        def addData(objId: CId, values: GObject) = {
          buffer2 += ((objId, values.toList.filter(mkFilter(values))))
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
      adder.outputRow = true
      cstore.foreach{case (id,v) => adder.addData(id, v)}
      adder.continue
      loopWhile(!adder.done) {
        reactWithin(0) (emergencyActions orElse {
          case TIMEOUT => 
            val (store1, md1) = I.multiStep(p, store, md, adder)
            store = store1
            md = md1
            if (buffer.size >= bufferSize || (System.currentTimeMillis - timeOfLastFlush) > minPlotUpdateInterval) flush
        })
      } andThen {
        sendChunk
        consumer ! Done(List("Time to run simulation: %.3fs".format((System.currentTimeMillis - startTime) / 1000.0)))
        //System.err.println("Total simulation time: " + ((System.currentTimeMillis - startTime) / 1000.0) + "s")
      }
    }
  }
}
