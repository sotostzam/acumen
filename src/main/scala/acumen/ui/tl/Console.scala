package acumen
package ui
package tl

import Logger._

class Console {

  var lastMessage: Msg = _

  def serializeMessage(msgType: String, textToSend: String = null, location: String = null, delOld: Boolean = false): Unit = {
    Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "console",
      "data" -> ujson.Arr(msgType,
        if (textToSend != null) textToSend else ujson.Null,
        if (location != null) location else ujson.Null,
        if (delOld) ujson.True else ujson.False))))
  }

  def append(instr: Instruction): Unit = instr match {
    case StatusUpdate(before, msg, after) =>
      lastMessage match {
        case Message(STATUS, oldMsg, _) if before =>
          lastMessage = Message(STATUS, TextMsg(oldMsg + msg + (if (after) "..." else "")))
          // delOld parameter here means that the last message displayed needs to be deleted.
          serializeMessage("status", oldMsg + msg + (if (after) "..." else ""), null, delOld = true)
        case _ =>
          lastMessage = Message(STATUS, TextMsg(msg + (if (after) "..." else "")))
          serializeMessage("status", msg + (if (after) "..." else ""))
      }
    case Separator =>
      serializeMessage("separator")
    case m: Msg =>
      lastMessage = m
      m match {
        case Message(ERROR, ExceptionMsg(e), pos) =>
          serializeMessage("error", ExceptionMsg(e).msg, pos.toString())
          System.err.println("This exception reported to console:")
          e.printStackTrace()
        case Message(INFO, message, _) =>
          serializeMessage("info", message.msg)
        case Message(STATUS, message, _) =>
          serializeMessage("status", message.msg)
          println("Status: " + message.msg)
        case _ => /* noop */
      }
  }
}
