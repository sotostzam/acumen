package acumen

import Stream._
import util.Canonical._
import ui.interpreter._
import Pretty._

/** Interface common to all interpreters. */
trait Interpreter {
  def newInterpreterModel : InterpreterModel
  def run(p:Prog) : InterpreterRes
  /** Map of parameters -> default values, visible to the user, e.g. in code area completions. */
  def visibleParameters(): Map[String,CValue]
}

abstract class InterpreterRes {
  def print : Unit;
  def printLast : Unit;
}

/** Used to store information about the Store. */
abstract class Metadata { def combine(that: Metadata): Metadata }
case object NoMetadata extends Metadata {
  def combine(that: Metadata): Metadata = that
}
case class SomeMetadata 
  ( hyp: Map[ (CId, ClassName, Option[String]), HypothesisOutcome ] /* (object, class, name) -> outcome */
  , timeDomain: (Double, Double)
  , rigorous: Boolean /* Does this describe output of a rigorous interpreter? */
  ) extends Metadata {
  def combine(that: Metadata): Metadata = {
    that match {
      case NoMetadata => this
      case SomeMetadata(th,tt,r) =>
        require( this.timeDomain._2 >= tt._1 || tt._2 >= this.timeDomain._1 
               , "Can not combine SomeMetadata with non-overlapping time domains.")
        SomeMetadata(
          (this.hyp.keySet union th.keySet).map(k => k -> {
            (this.hyp.get(k), th.get(k)) match {
              case (Some(o), None)    => o
              case (None, Some(o))    => o
              case (Some(l), Some(r)) => l pick r
          }}).toMap
        , (Math.min(this.timeDomain._1, tt._1), Math.max(this.timeDomain._2, tt._2))
        , r && rigorous)
    } 
  }
}
/** The result of evaluating a hypothesis.*/
trait HypothesisOutcome {
  /**
   * Returns either this or that outcome, depending on which is more significant. 
   * A failure is more significant than a success, and an earlier failure more
   * significant than a later one.
   */
  def pick(that: HypothesisOutcome): HypothesisOutcome
}
abstract class Success extends HypothesisOutcome { def pick(that: HypothesisOutcome) = that }
abstract class Failure(counterExample: Set[(Dot,CValue)]) extends HypothesisOutcome
/** Result of non-rigorous hypothesis evaluation (reference interpreter). */
case object TestSuccess extends Success
case class TestFailure(earliestTime: Double, counterExample: Set[(Dot,CValue)]) extends Failure(counterExample: Set[(Dot,CValue)]) {
  def pick(that: HypothesisOutcome) = that match {
    case TestSuccess    => this
    case f: TestFailure => if (this.earliestTime <= f.earliestTime) this else that
  }
}
/** Result of rigorous hypothesis evaluation (enclosure interpreter). */
case object CertainSuccess extends Success
abstract class RigorousFailure(earliestTime: (Double,Double), counterExample: Set[(Dot,CValue)]) extends Failure(counterExample: Set[(Dot,CValue)]) 
case class CertainFailure(earliestTime: (Double,Double), counterExample: Set[(Dot,CValue)]) extends RigorousFailure(earliestTime, counterExample) {
  def pick(that: HypothesisOutcome) = that match {
    case CertainSuccess      => this
    case _: UncertainFailure => this
    case f: CertainFailure   => if (this.earliestTime._1 <= f.earliestTime._1) this else that
  } 
}  
case class UncertainFailure(earliestTime: (Double,Double), counterExample: Set[(Dot,CValue)]) extends RigorousFailure(earliestTime, counterExample) {
  def pick(that: HypothesisOutcome): HypothesisOutcome = that match {
    case CertainSuccess      => this
    case f: UncertainFailure => if (this.earliestTime._1 <= f.earliestTime._1) this else that
    case f: CertainFailure   => f
  } 
}

/** Interface common to all interpreters whose results can be converted to/from CStores. */
trait CStoreInterpreter extends Interpreter {
  type Store

  override def newInterpreterModel = new CStoreModel(new CStoreOpts)

  def repr (s:Store) : CStore
  def fromCStore (cs:CStore, root:CId) : Store

  /** Based on prog, creates the initial store that will be used to start the simulation. */
  def init(prog:Prog) : (Prog, Store, Metadata)
  /**
   * Moves the simulation one step forward.  Returns None at the end of the simulation.
   * NOTE: Performing a step does not necessarily imply that time moves forward.
   * NOTE: The store "st" may be mutated in place or copied, depending on the interpreter.
   */
  def step(p:Prog, st:Store, md: Metadata) : Option[(Store, Metadata)]
  /** 
   * Performs multiple steps. Driven by "adder"  
   * NOTE: May be overridden for better performance.
   */
  def multiStep(p: Prog, st0: Store, md0: Metadata, adder: DataAdder) : (Store, Metadata) = {
    var st = st0
    var md = md0
    var cstore = repr(st0)
    var shouldAddData = ShouldAddData.IfLast 
    // ^^ set to IfLast on purpose to make things work
    while (true) {
      step(p, st, md) match {
        case Some((resSt,resMd)) => // If the simulation is not over
          cstore = repr(resSt) 
          shouldAddData = adder.newStep(getResultType(cstore))
          if (shouldAddData == ShouldAddData.Yes)
            cstore.foreach{case (id,obj) => adder.addData(id, obj)}
          if (!adder.continue)
            return (resSt,resMd)
          st = resSt
          md = resMd
        case None => // If the simulation is over
          if (shouldAddData == ShouldAddData.IfLast)
            cstore.foreach{case (id,obj) => adder.addData(id, obj)}
          adder.noMoreData()
          return (st,md)
      }
    }
    (st,md)
  }

  type History = Stream[Store]
  // Note: Even though a stream is a lazy list, it is used here more
  // like a traditional I/O stream; traversing more than once is only
  // supported if Store is an immutable data structure (as in the
  // reference interpreter).

  def fromCStore(st:CStore) : Store =
    fromCStore(st, mainId(st))

  def exposeExternally(store:Store, md:Metadata) : (Store, Metadata) =
    (store, md)

  /* main loop */
  def loop(p:Prog, st:Store, md:Metadata) : History = {
    st #:: (step(p, st, md) match {
        case None      => empty
        case Some((st1, md1)) => 
		      val (st2, md2) = exposeExternally(st1, md1)
		      loop(p, st2, md2)
      })
  }

  /* all-in-one main-loop */
  def run(p:Prog) = {
    val (p1,st,md) = init(p)
    val trace = loop(p1, st, md)
    CStoreRes(trace map repr)
  }


  /* multistep versions of loop and run */

  def loop(p:Prog, st:Store, md: Metadata, adder: DataAdder) : History = {
    st #:: { if (adder.done) empty
             else {
               val (st1, md1) = multiStep(p, st, md, adder)
               loop(p, st1, md1, adder)}
             }}

  def run(p: Prog, adder: DataAdder) : History = {
   val (p1,st,md) = init(p)
   loop(p1, st, md, adder)
  }
}

abstract class InterpreterCallbacks

trait RecursiveInterpreter extends Interpreter {
  def runInterpreter(prog:Prog, cb0: InterpreterCallbacks) : InterpreterRes
}

//
/* ************************************************************************ */
//

class CStoreOpts {
  var outputRows = OutputRows.WhenChanged
  var continuousSkip = 0
  var outputInternalState = false // controls "parent", "nextChild", "seed1", "seed2" but not "className"
  var outputSimulatorState = false // does not control Simulator.time, Simulator.resultType, or Simulator.endTime
  var keep3D = false
  //var outputPlottables = true 
  //var output3D = true 
  //var outputMisc = true // anything not covered by the above
}

/** 
 * Controls which rows are preserved when FilterDataAdder is used.
 * NOTE: A row here refers to the result of a call to Interpreter.step(),
 *       and corresponds to a row in the trace table.  
 */
object OutputRows extends Enumeration {
  val All, WhenChanged, FinalWhenChanged, ContinuousOnly, Last = Value
}

//
/* ************************************************************************ */
//

/*
 * An interface used by CStoreInterpreter#multiStep to output simulation
 * results.
 * 
 * The protocol is as follows:
 * 
 * After each step multiStep calls DataAdder#newStep with the current
 * ResultType for that step if there is data or DataAdder#noMoreData
 * if the simultation has
 * ended.  Assuming the simulation has not ended, than the result of
 * newStep determine if data should be added for this step,  if so
 * than DataAdder#addData is called for each object in the store.
 * After the data is optionally added
 * DataAdder#continue is called to determine if multiStep should
 * perform another step or return.  The result of multiStep is the
 * store of the last step.  If the simulation has ended the result
 * is the store for the final step and DataAdder#done is set to true.
 * 
 * The normal usage of DataAdder with multiStep is an alternative to
 * single stepping where data is returned via addData rather than the
 * conversion to a CStore after every step.  However, DataAdder can
 * also be used just to filter rows and still use the CStore for the
 * rows returned, see FilterRowsDataAdder for an example on how to use
 * this
 */
abstract class DataAdder {
  /** After multiStep returns, this value can be checked to see if the
   * simulation is done. */
  var done = false
  /** 
   * Called to register that a step has been performed by an interpreter with
   * ResultType "t".
   * The return value indicates if the data should then be added using addData.
   * A naive implementation is allowed to ignore the return value and call
   * addData every time.
   */
  def newStep(t: ResultType) : ShouldAddData.Value
  /** 
   * Called when the simulation has ended and no more data should be added.
   */
  def noMoreData() : Unit = {done = true}
  /** 
   * Called to update the data collected for the object corresponding to "objId"
   * in this adder with "values".
   */
  def addData(objId: CId, values: GObject) : Unit
  /**
   * Called after each step.  If false than multiStep should not continue and
   * return the current Store.
   */
  def continue : Boolean 
}

/** 
 * Returned by DataAdder.newStep(...) and used to tell CStoreInterpreter.multiStep() 
 * whether or not to add the data. IfLast refers to the very last step of the
 * simulation.
 */
object ShouldAddData extends Enumeration {
  val Yes, IfLast, No = Value
}

//
/* ************************************************************************ */
//

class StopAtFixedPoint extends DataAdder {
  var curStepType : ResultType = Discrete
  def newStep(t: ResultType) : ShouldAddData.Value = {curStepType = t; ShouldAddData.No}
  def addData(objId: CId, values: GObject) {}
  def continue = curStepType != FixedPoint
}

abstract class FilterDataAdder(var opts: CStoreOpts) extends DataAdder {
  var prevStepType : ResultType = Discrete
  var curStepType : ResultType = Discrete
  var outputRow : Boolean = false
  var contCountdown : Int = 0
  def newStep(t: ResultType) = {
    curStepType = t
    import OutputRows._
    val what = opts.outputRows
    outputRow = (what == All // 1
                 || (curStepType == Continuous && what != Last)  // 2
                 || (curStepType ==  Discrete && what == WhenChanged) // 3
                 || (prevStepType == Discrete && curStepType == FixedPoint && what == FinalWhenChanged)) // 4
    //                   <Last> @Continuous @Discrete <FP. Changed> @FixedPoint
    // All                 1         1 2        1          1             1 
    // WhenChanged         y          2         3          n             n
    // FinalWhenChanged    y          2         n          4             n
    // ContinuousOnly      y          2         n          n             n 
    // Last                y          n         n          n             n 
    prevStepType = curStepType
    if (curStepType == Continuous) {
      if (contCountdown == 0) {
        contCountdown = opts.continuousSkip
      } else {
        outputRow = false
        contCountdown -= 1
      }
    }
    if (outputRow)         ShouldAddData.Yes
    else if (what == Last) ShouldAddData.IfLast
    else                   ShouldAddData.No
  }
  def mkFilter(e:GObject) : ((Name, GValue)) => Boolean = {
    val VClassName(ClassName(name)) = e.find{_._1 == Name("className",0)}.get._2
    if (name == "Simulator" && !opts.outputSimulatorState)
      { case (x,v) => x.x == "className" || x.x == "time" || x.x == "endTime" || x.x == "resultType"}
    else if (!opts.outputInternalState)
      { case (x,_) => x.x == "className" || interpreters.Common.specialFields.indexOf(x.x) == -1 }
    else
      { case (_,_) => true }
  }
  //def sortObject(e:GObject) = {
  //  e.toList.sortWith { (a,b) => a._1 < b._1 } 
  //}
}

class FilterRowsDataAdder(opts: CStoreOpts) extends FilterDataAdder(opts) {
  override def newStep(t: ResultType) = {super.newStep(t); ShouldAddData.No}
  def addData(objId: CId, values: GObject) = {}
  def continue = !outputRow 
}

class DumpSample(out: java.io.PrintStream) extends DataAdder {
  val pp = new Pretty
  pp.filterStore = true
  pp.predictableDoubles = true
  var stepNum = -1
  var discrStepNum = -1
  var last : CStore = null
  var prevStepType : ResultType = Discrete
  var curStepType : ResultType = Discrete
  var useResult : ShouldAddData.Value =  ShouldAddData.No
  var store = new scala.collection.mutable.MutableList[(CId, GObject)]
  
  def dumpLastStep = {
    out.println(pp.pprint(pp.prettyStore(store)))
    out.println("-" * 30 + stepNum)
  }
  
  def newStep(t: ResultType) = {
    if (useResult == ShouldAddData.Yes)
      dumpLastStep
    store.clear()
    curStepType = t
    useResult = ShouldAddData.IfLast
    if (prevStepType == Discrete && curStepType == FixedPoint) {
      stepNum += 1
      discrStepNum += 1
      if (discrStepNum < 4)
        useResult = ShouldAddData.Yes
    } else if (curStepType == Continuous) {
      stepNum += 1
    }
    prevStepType = curStepType
    useResult
  }
  override def noMoreData() = {
    super.noMoreData()
    dumpLastStep
  }
  def addData(objId: CId, values: GObject) = {
    store += ((objId, values.toList)) // need to make a copy of the values as they could change
  }
  def continue = true
}

//
/* ************************************************************************ */
//

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

  def dumpContinuous(out: java.io.PrintStream) = {
    val pp = new Pretty
    pp.filterStore = true
    def dumpStep(st: CStore) = {
      out.println(pp.pprint(pp.prettyStore(st)))
    }
    loop { (st, n) => 
      if (n == Continuous)
        dumpStep(st)
    }
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

