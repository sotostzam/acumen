package acumen

import Stream._
import util.Canonical._
import acumen.ui.AbstractTraceModel

trait Interpreter {

  def newTraceModel : AbstractTraceModel
  
  def generateTraceModel(text: String): AbstractTraceModel = null

  type Store
  def repr (s:Store) : CStore
  def fromCStore (cs:CStore, root:CId) : Store
  def init(prog:Prog) : (Prog, Store)
  def step(p:Prog, st:Store) : Option[Store]

  type History = Stream[Store]

  def fromCStore(st:CStore) : Store =
    fromCStore(st, mainId(st))

  /* main loop */
  def loop(p:Prog, st:Store) : History = {
    st #:: (step(p, st) match {
        case None      => empty
        case Some(st1) => loop(p, st1)
      })
  }

  /* all-in-one main-loop */
  def run(p:Prog) : History = {
    val (p1,st) = init(p)
    loop (p1, st)
  }

}
