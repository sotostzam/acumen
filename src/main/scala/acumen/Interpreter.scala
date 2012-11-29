package acumen

import Stream._
import util.Canonical._
import ui.interpreter._
import Pretty._

abstract class InterpreterRes {
  def print : Unit;
  def printLast : Unit;
}

trait Interpreter {
  def newInterpreterModel : InterpreterModel
  def run(p:Prog) : InterpreterRes
}

case class CStoreRes(ctrace: Stream[CStore]) extends InterpreterRes {
  def print = {
    var i = 0
    for (st <- ctrace) {
      println(pprint(prettyStore(st)))
      println("-" * 30 + i)
      i += 1
    }
  }
  def printLast = {
    println(pprint(prettyStore(ctrace.last)))
  }
}

trait CStoreInterpreter extends Interpreter {
  type Store
  def repr (s:Store) : CStore

  override def newInterpreterModel = new CStoreModel
  
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
  def run(p:Prog) = {
    val (p1,st) = init(p)
    val trace = loop (p1, st)
    CStoreRes(trace map repr)
  }
}

abstract class InterpreterCallbacks

trait RecursiveInterpreter extends Interpreter {
  def runInterpreter(prog:Prog, cb0: InterpreterCallbacks) : InterpreterRes
}
