package acumen
package ui

import collection.immutable.Queue
import scala.actors._
import InterpreterModel._

class CStoreInterpreter(val interpreter: Interpreter) extends InterpreterModel {

  def newTraceModel = interpreter.newTraceModel

  def init(progText: String, consumer:Actor) = new InterpreterActor(progText, consumer) {

    var buffer = Queue.empty[CStore]
    var defaultBufferSize = 200
    var bufferSize = defaultBufferSize

    def sendChunk {
      val toSend = if (buffer.isEmpty) null else CStoreTraceData(buffer)
      consumer ! Chunk(toSend)
      buffer = Queue.empty[CStore]
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
      })
    }

    def produce : Unit = {
      val I = interpreter
      val (p, store) = I.init(prog)
      val cstore = I.repr(store)
      // Send (what I belive is --kevina) the initialization Chunk
      buffer enqueue cstore
      flush andThen {
        val trace = I.loop(p, I.fromCStore(cstore)) map I.repr
        val iter = trace.iterator
        loopWhile(iter.hasNext) {
          reactWithin(0) (emergencyActions orElse {
            case TIMEOUT => 
              buffer = buffer enqueue iter.next
            if (buffer.size >= bufferSize) flush
          })
        } andThen {
          sendChunk
          consumer ! Done
        }
      }
    }
  }
}
