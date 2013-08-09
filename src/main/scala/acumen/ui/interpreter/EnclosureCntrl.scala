package acumen
package ui
package interpreter

import collection.mutable.ArrayBuffer
import scala.actors._
import acumen.interpreters.enclosure.EnclosureInterpreterCallbacks
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import InterpreterCntrl._

class EnclosureCntrl(val interpreter: RecursiveInterpreter) extends InterpreterCntrl {

  def newInterpreterModel = interpreter.newInterpreterModel

  def init(progText: String, consumer:Actor) = new InterpreterActor(progText, consumer) {

    val callbacks = new EnclosureInterpreterCallbacks {
      // Bouncing Ball Example:
      // With buffer size = 12
      //   Time to run simulation: 32.566000
      //   Time to run simulation: 27.994000
      //   Time to run simulation: 28.202000
      // With buffer size = 1
      //   Time to run simulation: 33.578000
      //   Time to run simulation: 29.054000
      //   Time to run simulation: 28.657000
      val defaultMaxBufSize = 1
      var buf = new ArrayBuffer[UnivariateAffineEnclosure]
      var maxBufSize = 1 // start off with one step

      def sendChunk {
        if (buf.isEmpty) {
          consumer ! Chunk(null)
        } else {
          consumer ! Chunk(new EnclosureTraceData(buf, endTime))
          buf = new ArrayBuffer[UnivariateAffineEnclosure]
        }
      }

      val emergencyActions : PartialFunction[Any,Unit] = {
        case Stop => { sendChunk; exit }
        case Flush => flush
      }

      def awaitNextAction {
        receive (emergencyActions orElse {
          case GoOn => maxBufSize = defaultMaxBufSize
          case Step => maxBufSize = 1
          case msg  => println("Unknown Message Recived by Enclosure Intr.: "  + msg)
        })
      }
      
      def flush {
        sendChunk
        awaitNextAction
      }

      def log(msg: String) : Unit = {
        if (msg != null)
          emitProgressMsg(msg)
      }

      override def sendResult(d: Iterable[UnivariateAffineEnclosure]) {
        if (maxBufSize == 1) {
          consumer ! Chunk(new EnclosureTraceData(d, endTime))
          awaitNextAction
        } else {
          buf ++= d
          receiveWithin(0)(emergencyActions orElse {
            case TIMEOUT => if (buf.size > maxBufSize) flush
          })
        }
      }
    }

    override def parse() = {
      val ast = Parser.run(Parser.prog, progText)
      //val dif = SD.run(ast)
      val des = Desugarer.run(ast)
      val des2 = if (Main.extractHA)
                   new Extract(des).res
                 else
                   des
      prog = des2
    }

    def produce : Unit = {
      val s = System.currentTimeMillis
      interpreter.runInterpreter(prog, callbacks)
      consumer ! Done
      println("Time to run simulation: %f".format((System.currentTimeMillis - s)/1000.0))
    }
  }
}
