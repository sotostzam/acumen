package acumen
package ui

import scala.actors._

object Supervisor extends DaemonActor {
  case class Link(a: AbstractActor, n: String, restart: ()=>Unit)
  val watching = new collection.mutable.ListMap[OutputChannel[Any],(String,()=>Unit)]
  def act() = {
    trapExit = true
    //println("Supervisor Actor Starting.")
    loop {react {
      case Link(a,s,rs) =>
        //println("Linking with " + s)
        watching += ((a,(s,rs)))
        link(a)
      case Exit(a,ue:UncaughtException) =>
        val (name,restart) = watching.getOrElse(sender, ("An Unknown",null))
        println("*** " + name + " actor Died Unexpected!")
        ue.cause.printStackTrace()
        unlink(a)
        if (restart != null) {
          println("*** Restating " + name + ".")
          restart()
        }
      case Exit(a,_) =>
        val (name,_) = watching.getOrElse(sender,("An Unknown", null))
        println("*** " + name + " actor Terminated!")
        unlink(a)
      case msg =>
        println("*** " + "Supervisor Actor: Unknown msg: " + msg)
    }}
  }
  start()
  // watch the calling actor
  def watch(a: AbstractActor, n: String, restart: ()=>Unit = null) =
    this ! Link(a,n,restart)
}

