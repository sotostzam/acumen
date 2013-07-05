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
  
  def loop(action: (CStore, ResultType) => Unit) : Unit = {
    var prevStepType : ResultType = Discrete
    var nextContinuous = true
    for (st <- ctrace) {
      val VResultType(curStepType) = st.get(CId(0)).orNull.get(Name("resultType",0)).orNull
      val resultType = 
        if (prevStepType == Discrete && curStepType == FixedPoint)
          Some(FixedPoint)
        else if (curStepType == Continuous)
          Some(Continuous)
        else
          None
      resultType.foreach{ n => action(st, n) }
      prevStepType = curStepType
    };
  }

  def dumpSample(out: java.io.PrintStream) = {
    val pp = new Pretty
    pp.filterStore = true
    var stepNum = -1
    var discrStepNum = -1
    var last : CStore = null
    def dumpStep(st: CStore) = {
      out.println(pp.pprint(pp.prettyStore(st)))
      out.println("-" * 30 + stepNum)
    }
    loop { (st, n) => 
      stepNum += 1
      if (n == FixedPoint) {
        discrStepNum += 1
        if (discrStepNum < 4)
          dumpStep(st)
      }
      last = st
    }
    dumpStep(last)
  }

  // Note: currently unused, here in case it is useful
  def dumpRandomSample(out: java.io.PrintStream) = {
    val pp = new Pretty
    pp.filterStore = true
    var i = 0
    var j = 0
    loop { (st, n) => 
      if (n == Continuous) i += 1
      if (n == FixedPoint) j += 1
    }
    // best to use prime numbers
    var c_every = if (i <= 11) 1 else i / 11;
    var d_every = if (j <= 7) 1 else j / 7;
    var last = j;
    i = 0
    j = 0
    loop { (st, n) =>
      var skip = false
      if (n == Continuous) {
        skip = i % c_every != 0
        i += 1
      } else if (n ==  FixedPoint) {
        skip = j % d_every != 0
        j += 1
      }
      if (!skip) {
        out.println(pp.pprint(pp.prettyStore(st)))
        out.println("-" * 30 + n)
      }
    }
  }
  def printLast = {
    println(pprint(prettyStore(ctrace.last)))
  }
}

object CStoreOpts {
  object OutputRows extends Enumeration {
    val AllRows, DiscreteWhenChanged, ContinuousOnly, Last = Value
  }
}

class CStoreOpts {
  import CStoreOpts._
  var outputRows = OutputRows.AllRows
  var outputInternalState = true // controls "parent", "nextChild", "seed1", "seed2"
  var outputSimulatorState = true // does not control Simulator.time and Simulator.stepType
  var outputNonPlottables = true // 
  def outputAllRows = outputRows == OutputRows.AllRows
  def outputSomeDiscrete = outputRows == OutputRows.AllRows || outputRows == OutputRows.DiscreteWhenChanged
  def outputLastOnly = outputRows == OutputRows.Last
}

trait CStoreInterpreter extends Interpreter {
  type Store
  def repr (s:Store) : CStore

  override def newInterpreterModel = new CStoreModel
  
  def fromCStore (cs:CStore, root:CId) : Store
  def init(prog:Prog, opts: CStoreOpts) : (Prog, Store)
  def step(p:Prog, st:Store) : Option[Store]

  type History = Stream[Store]

  def fromCStore(st:CStore) : Store =
    fromCStore(st, mainId(st))

  def expose_externally(store:Store) : Store = {
    store
  }

  /* main loop */
  def loop(p:Prog, st:Store) : History = {
    st #:: (step(p, st) match {
        case None      => empty
        case Some(st1) => 
		      val st2 = expose_externally(st1)
		      loop(p, st2)
      })
  }

  /* all-in-one main-loop */
  def run(p:Prog) = {
    val (p1,st) = init(p, new CStoreOpts)
    val trace = loop (p1, st)
    CStoreRes(trace map repr)
  }
}

abstract class InterpreterCallbacks

trait RecursiveInterpreter extends Interpreter {
  def runInterpreter(prog:Prog, cb0: InterpreterCallbacks) : InterpreterRes
}
