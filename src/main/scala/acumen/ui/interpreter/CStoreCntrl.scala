package acumen
package ui
package interpreter

import collection.immutable.Queue
import collection.mutable.ListBuffer
import scala.actors._
import InterpreterCntrl._

class CStoreCntrl(val interpreter: CStoreInterpreter) extends InterpreterCntrl {

  def newInterpreterModel = interpreter.newInterpreterModel

  def init(progText: String, consumer:Actor) = new InterpreterActor(progText, consumer) {

    var buffer = Queue.empty[GStore]
    var defaultBufferSize = 200
    var bufferSize = 1 // start off with one step

    override def parse() = 
      if (interpreter.id contains "original") {
        val ast = Parser.run(Parser.prog, progText)
        val dif = SD.run(ast)
        // transform ODEs the old-fashioned way in the original interpreter
        val des = Main.applyPasses(dif, Seq("desugar-local"))
        prog = des
      } else if (interpreter.id contains "reference") {
        val ast = Parser.run(Parser.prog, progText)
        val dif = SD.run(ast)
        // transform ODEs the old-fashioned way (but with in-lining) in the experimental interpreter
        val des = Main.applyPasses(dif, Seq("desugar-local-inline"))
        prog = des
      } else super.parse
    
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
      react (emergencyActions orElse {
        case GoOn => bufferSize = defaultBufferSize
        case Step => bufferSize = 1
        case msg => println("Unknown msg received by producer: " + msg)
      })
    }

    def produce : Unit = {
      val startTime = System.currentTimeMillis
      val I = interpreter
      val (p, store0) = I.init(prog)
      var store = I.multiStep(p, store0, new StopAtFixedPoint)
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
            store = I.multiStep(p, store, adder)
            if (buffer.size >= bufferSize) flush
        })
      } andThen {
        sendChunk
        consumer ! Done(List("Time to run simulation: %.3fs".format((System.currentTimeMillis - startTime) / 1000.0)))
        //System.err.println("Total simulation time: " + ((System.currentTimeMillis - startTime) / 1000.0) + "s")
      }
    }
  }
}
