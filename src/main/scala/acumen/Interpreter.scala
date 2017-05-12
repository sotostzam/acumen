package acumen

import annotation.tailrec
import collection.JavaConversions.iterableAsScalaIterable
import Stream._
import util.Canonical._
import util.Names._
import ui.interpreter._
import Pretty._
import acumen.Errors._
import acumen.interpreters.Common._
import acumen.interpreters.enclosure.{IntervalSplitter, SplitInterval}
import acumen.util.Conversions
import acumen.interpreters.enclosure.Interval

import scala.collection.mutable

/** Interface common to all interpreters. */
trait Interpreter {
  def interpreterType : InterpreterType
  def newInterpreterModel : InterpreterModel
  def lift: Prog => Prog
  def run(p: Prog): InterpreterRes
  def streamingRun(p: Prog): InterpreterRes
  /** Map of parameters -> default values, visible to the user, e.g. in code area completions. */
  def visibleParameters(): Map[String,CValue]
}

abstract class InterpreterRes {
  def print : Unit;
  def printLast : Unit;
  def metadata : Metadata;
}

/** Interface common to all interpreters whose results can be converted to/from CStores. */
trait CStoreInterpreter extends Interpreter {
  type SuperStore = Map[Tag, Store]
  type SuperMetadata = Map[Tag, Metadata]
  type Store

  override def newInterpreterModel = new CStoreModel(new CStoreOpts)

  def repr (s:Store) : CStore
  def fromCStore (cs:CStore, root:CId) : Store

  /** Based on prog, creates the initial store that will be used to start the simulation. */
  def init(prog:Prog) : (Prog, SuperStore, SuperMetadata)
  
  /** Based on the current interpreter, prog will be lifted differently  */
  def lift ():Prog => Prog

  /**
    * Generic splitting function relying on a convertion to CStore and back to Store then
    * @param st The store to split
    * @return The SuperStore containing the result of the split
    */
  def splitIntervalsStore(st: Store): SuperStore = {
    def splitIntervals(cs: CStore): Map[Tag, CStore] = {
      //Find intervals in a CStore and produce a list of updates to apply
      def findIntervals(cs: CStore): List[((CId, Name), IntervalSplitter)] = {
        //return the interval in the object field "fields"
        (cs flatMap { case (id, o) => o flatMap {
          case (n, VLit(GInterval(SplitInterval(_, is)))) => Some((id, n), is)
          //Shouldn't happen, would lead to an interpreter error
          case (n, VLit(GInterval(_))) => println("Basic intervals are prohibited in the optimized interpreter"); None
          //@Masoumeh>
          case (n, VLit(GIntEnclosure(s))) => 
            println("GIntEnclosure"); None
          case (n, VLit(GBoolEnclosure(s))) => 
            println("GBoolEnclosure"); None 
          //<@Masoumeh
          case _ => None
          }
        }).toList
      }

      //Build the list of updates
      val intervalsToSplit = findIntervals(cs)
      //Apply each update to every Object resulting from the previous updates
      intervalsToSplit.foldLeft(Map(Tag.root -> cs)) { 
          case (res, ((id, n), is)) =>
                for ((tag, st) <- res; (p, i) <- is.subPoints.map(_.doubleValue()).zipWithIndex) yield
                  ((id, n, i, is.subPoints.size - 1) :: tag) -> st.updated(id, st(id) updated(n, VLit(GDouble(p))))
          
      }
    }
    /* General splitting function which relies on the repr / fromCStore pseudo reciprocity
     * It might by usefull to overload it to perform splitting elsewhere than at the init step or not to use points */
    splitIntervals(repr(st)) mapValues (fromCStore(_, CId.nil))
  }

  /**
    * Read the deadStore flag in a store. This flag is set to true if something which should have crashed the simulation happen
    * NOTE: Override for performance
    * @param cs the tested store
    * @return true is the store is dead, false otherwise
    */
  def isDead(cs: Store): Boolean = {
    acumen.util.Conversions.extractBoolean(
      acumen.util.Canonical.getInSimulator(Name("deadStore", 0), repr(cs)))
  }

  //FIXME: Should be replaced by an exception mechanism because NaN an Infinity are not considered as invalid value in Acumen
  /** return true if an illegal value appear in the Store */
  def containsIllegalValues(st: Store) = {
    repr(st) exists { case(id, o) => o exists {
      //case (n, VLit(GDouble(v))) => v.isNaN || v.isInfinity
      case _ => false
    }}
  }

  sealed abstract class StepRes
  case class Data(st: SuperStore, md: SuperMetadata) extends StepRes
  case class Done(md: SuperMetadata, endTime: Double) extends StepRes
  /**
   * Moves the simulation one step forward.  Returns Done at the end of the simulation.
   * NOTE: Performing a step does not necessarily imply that time moves forward.
   * NOTE: The store "st" may be mutated in place or copied, depending on the interpreter.
   */
  def step(p:Prog, st:Store, md: Metadata) : StepRes
  /** 
   * Performs multiple steps. Driven by "adder"  
   * NOTE: May be overridden for better performance.
   */
  def multiStep(p: Prog, st0: Store, md0: Metadata, adder: DataAdder, baseTag: Tag = Tag.root) : Map[Tag, (Store, Metadata, Double)] = {
    //The tags in the returned map are absolute (baseTag added before the return)
    var st = st0
    var md = md0
    var cstores = Map(Tag.root -> repr(st0))
    /* - The Initial store is output on a higher level
     * - The user set simulator parameters are evaluated
     *   in the first Discrete step that is output depending
     *   on Common/initStoreTxt
     * - Thereafter, the DataAdder decides what should be output */
    adder.shouldAddData = adder.initialShouldAddData
    while (true) {
      step(p, st, md) match {
        case Data(resSSt,resSMd) => // If the simulation is not over
          cstores = resSSt map {case (t, st) => (t::baseTag) -> repr(st)}
          val deadStores = resSSt map {case (t, st) => (t::baseTag) -> isDead(st)}
          // The same data adder is used for the possible splits resulting from an original
          // store here because the duplication of DataAdder is impossible.
          // The adders map will be reconstructed by produce in respect of the returned map.
          cstores foreach {case (t, cst) =>
            adder.shouldAddData = adder.newStep(getResultType(cst))
            //Dead Stores are added to see that something wrong happened in the table. The plotter can deal with it.
            if (adder.shouldAddData == ShouldAddData.Yes)
              addData(cst, adder, t, deadStores(t))
          }
          if (!adder.continue) {
            val res = resSSt.keys map (t => (t::baseTag) -> (resSSt(t), resSMd(t), Double.NaN))
            return res.toMap
          } else if(resSSt.size == 1){
            //If no split occur, then we can continue. If some happen, then it is impossible to continue.
            st = resSSt.values.head
            md = resSMd.values.head
          } else throw ShouldNeverHappen()
        case Done(resSMd,endTime) => // If the simulation is over
          if (adder.addLast)
            addData(cstores.values.head, adder, baseTag, isDead(st0))
          adder.noMoreData()
          return resSMd map {case(t, md) => (t::baseTag) -> (st, md, endTime)}
      }
    }
    throw  ShouldNeverHappen()
  }

  type History = Stream[Store]
  // Note: Even though a stream is a lazy list, it is used here more
  // like a traditional I/O stream; traversing more than once is only
  // supported if Store is an immutable data structure (as in the
  // reference interpreter).

  def fromCStore(st:CStore) : Store =
    fromCStore(st, mainId(st))

  def exposeExternally(store:Store, md:Metadata) : (Store, Metadata) =
    if (Main.serverMode) {
      val json1 = JSon.toJSON(repr(store)).toString
      val store2 = JSon.fromJSON(Main.send_recv(json1))
      (fromCStore(store2), md) // FIXME add support for metadata
    }
    else (store, md)

  /* main loop */
  /* Note: returns a Stream (i.e. lazy list), but does not
   * support returning MetaData as there isn't an easy
   * way to do that without forcing evaluation of the
   * entire list */
  def lazyLoop(p:Prog, st:Store, md:Metadata = NoMetadata) : History = {
    st #:: (step(p, st, md) match {
        case Done(_,_)      => empty
        case Data(sst1, smd1) =>
          require(sst1.size == 1, "A step returned more than one Store. Splitting is prohibited in lazyLoop.")
          val (st2, md2) = exposeExternally(sst1.values.head, smd1.values.head)
          lazyLoop(p, st2, md2)
      })
  }

  /* all-in-one main-loop */
  def lazyRun(p:Prog): CStoreRes = {
    val (p1,sst,mds) = init(p)
    require(sst.size == 1, "More than one store returned by init. Splitting is prohibited in this interpreter.")
    val trace = lazyLoop(p1, sst.values.head, mds.values.head)
    CStoreRes(trace map repr, NoMetadata)
  }

  /* generic non lazy version of run that support returning metadata */

  def run(p:Prog) : InterpreterRes = {
    val (trace,md) = run(p, SingleStep);
    CStoreRes(trace dropRight(1) map repr, md)
  }

  /* multistep versions of loop and run, not lazy but supports
   * returning metadata */

  def loop(p:Prog, st:Store, md: Metadata, adder: DataAdder, tag: Tag): (History, Metadata) = {
    @tailrec def loopInner(st0: Store, md0: Metadata, h: => History): (History, Metadata) =
      if (adder.done) (h, md0)
      else {
        //Dynamic split prohibited here
        val multiStepRes = multiStep(p, st0, md0, adder, tag)
        require(multiStepRes.size == 1, "A step retuned more than one store. Dynamic splitting in prohibited in the loop function.")
        multiStepRes.head match  { case (tag, (st1, md1, _)) =>
          val (st2, md2) = exposeExternally(st1, md1)
          loopInner(st2, md2, st2 #:: h)
        }
      } 
    loopInner(st, md, empty)
  }

  def run(p: Prog, adder: DataAdder) : (History,Metadata) = {
   val (p1,sst,mds) = init(p)

    val simResult  = sst map { case (tag, st) =>
      loop(p1, st, mds(tag), adder, tag)
    }
    // FIXME: Changing the signature of this function has broad consequences but is required to be consistent with the splitting
    // But it seems that the resturned value is not used for the res file, which make this implementation sufficient for testing
    simResult.head
  }

  /* streaming version of loop. does not preserve history. */

  def streamingRun(p: Prog): InterpreterRes = {
    def streamingLoop(p: Prog, st: Store, md: Metadata, adder: DataAdder): (Store, Metadata) = {
      @tailrec def loopInner(st0: Store, md0: Metadata): (Store, Metadata) =
        if (adder.done) (st0, md0)
        else {
          //It is assumed that the data returned by multistep is a one element map.
          val (st1, md1, _) = multiStep(p, st0, md0, adder).values.head
          val (st2, md2) = exposeExternally(st1, md1)
          loopInner(st2, md2)
        }
      loopInner(st, md)
    }
    val (p1, st, md) = init(p)
    val (lastStore, md1) = streamingLoop(p1, st.values.head, md.values.head, SingleStep)
    CStoreRes(repr(lastStore) #:: empty, md1)
  }

  /**
    * Generic method to buffer data from a SuperStore before sending it to the model
    * @param sst the SuperStore to be added
    * @param adders the adders corresponding to the stores in sst
    */
  def addData(sst: SuperStore, adders: collection.mutable.Map[Tag, DataAdder]): Unit = {
    sst.keys foreach (tag => addData(sst(tag), adders(tag), tag))
  }

  /**
    * Generic method to buffer data from a Store before sending it to the model
    * @param st the store to be added
    * @param adder the adder to use to add st
    * @param tag the tag attached to st
    */
  def addData(st: Store, adder: DataAdder, tag: Tag): Unit =
    addData(repr(st), adder, tag, isDead(st))

  /**
    * Generic method to buffer data from a CStore before sending it to the model
    * @param cStore the CStore to be added
    * @param adder the adder to use to add cStore
    * @param tag the tag attached to cStore
    */
  def addData(cStore: CStore, adder: DataAdder, tag: Tag, deadTag: Boolean): Unit =
    cStore.foreach { case (id, o) => adder.addData(id, o, tag, deadTag) }
  
}

abstract class InterpreterCallbacks

trait RecursiveInterpreter extends Interpreter {
  def runInterpreter(prog:Prog, cb0: InterpreterCallbacks) : InterpreterRes
}

//
/* ************************************************************************ */
//

class CStoreOpts {
  var outputRows = OutputRows.All
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
  def addData(objId: CId, values: GObject, tag: Tag, deadTag: Boolean) : Unit
  /**
   * Called after each step.  If false than multiStep should not continue and
   * return the current Store.
   */
  def continue : Boolean
  /**
   *  Decides how the data adding flag is initialized
   *  it might become relevant (depending on addLast) 
   *  if multistep immediately reaches Done
   */
  val initialShouldAddData = ShouldAddData.No
  /**
   *  Data adding flag
   */
  var shouldAddData = ShouldAddData.No
  /**
   * Called to check if the last Store should be forcefully output
   */
  def addLast : Boolean = false
}

/** 
 * Returned by DataAdder.newStep(...) and used to tell CStoreInterpreter.multiStep() 
 * whether or not to add the data. IfLast refers to the very last step of the
 * simulation.
 */
object ShouldAddData extends Enumeration { // Legacy, used values are Yes/No -> Boolean
  val Yes, IfLast, No = Value
}

//
/* ************************************************************************ */
//

/**
 * A data adder that only takes a single step each time. */
case object SingleStep extends DataAdder {
  def newStep(t: ResultType) : ShouldAddData.Value = ShouldAddData.No
  def addData(objId: CId, values: GObject, tag: Tag = Tag.root, deadTag: Boolean = false) {}
  def continue = false
}

class StopAtFixedPoint extends DataAdder {
  var curStepType : ResultType = Discrete
  def newStep(t: ResultType) : ShouldAddData.Value = {curStepType = t; ShouldAddData.No}
  def addData(objId: CId, values: GObject, tag: Tag = Tag.root, deadTag: Boolean = false) {}
  def continue = curStepType != FixedPoint
}

abstract class FilterDataAdder(var opts: CStoreOpts) extends DataAdder {
  var prevStepType : ResultType = Initial
  var curStepType : ResultType = Initial
  var outputRow : Boolean = false
  var contCountdown : Int = 0

  override def addLast : Boolean = {
    import OutputRows._
    opts.outputRows == Last || opts.outputRows == ContinuousOnly
  }
  
  def newStep(t: ResultType) = {
    curStepType = t
    import OutputRows._
    val what = opts.outputRows
    outputRow = (   (                                                         what == All)               // 1
                 || (                            curStepType == Continuous && what != Last)              // 2
                 || (                            curStepType == Discrete   && what == WhenChanged)       // 3
                 || (prevStepType == Discrete && curStepType == FixedPoint && what == FinalWhenChanged)) // 4
    /*                   <Initial> [1st @Discrete] @Discrete @FixedPoint @Continuous <Last '15   '14> <Last Forced>
     * All                   y            y            y          y           y              y    y       n 
     * WhenChanged           y            y            y          n           y              n+   y       n
     * FinalWhenChanged      y            y            n          4           y              4+   2 4     n
     * ContinuousOnly        y            y            n          n           y              n    2       y
     * Last                  y            y            n          n           n              n    n       y
     * 
     * - The user set simulator parameters are processed in the 1st Discrete step (if there is any).
     * - The '+' marks under Last '15 signal that even if the (technically) last Store is not output by
     *   the filtering above, a preceding one, that is identical to it except for the resultType, is.
     * - This is guaranteed under Last '14 as well if simulator.endTime != 0 
     * - The last Store is forcefully output during multiStep if the addLast member function returns true
     */

    prevStepType = curStepType
 
    // Skips reporting opts.continuousSkip number of continuous segments
    if (curStepType == Continuous) {
      if (contCountdown == 0) {
        contCountdown = opts.continuousSkip
      } else {
        outputRow = false
        contCountdown -= 1
      }
    }

    if (outputRow)         ShouldAddData.Yes
    else if (what == Last) ShouldAddData.IfLast // Legacy
    else                   ShouldAddData.No
  }
  def mkFilter(e:GObject, plotFilter: List[Name]) : ((Name, GValue)) => Boolean = {
    val VClassName(ClassName(name)) = e.find{_._1 == Name("className",0)}.get._2
    if (name == "Simulator" && !opts.outputSimulatorState)
      { case (x,v) => x.x == "className" || x.x == "time" || x.x == "endTime" || x.x == "resultType" || x.x == "variableCount" }
    else if (!opts.outputInternalState)
      { case (x,_) => variableFilter(x.x, x.primes, plotFilter) }
    else
      { case (_,_) => false }
  }

  /** Filter the variables that need to be plotted.
    * If the _plot not exists, plot everything except the specialFields.
    * Else if the _plot exists but it is empty, then not plot anything.
    * Otherwise, plot the variables that contained in the _plot. */
  def variableFilter(variableName: String, primes: Int, plotFilter: List[Name]): Boolean = {
    variableName match {
      case "className" => true
      case "_plot" => false
      case _ =>
        if (interpreters.Common.specialFields.contains(variableName)) false
        else if (plotFilter == null) true
        else if (plotFilter.isEmpty) false
        else { // check the variable name is contained in the _plot variable
          if (plotFilter.contains(Name(variableName, primes))) true
          else false
        }
    }
  }
  /** Get the variable names from _plot variable */
  def getPlotFilter(e: GObject): List[Name] = {
    // All the names in this GObject, the variable names in _plot should be contained in the list
    val names = e.unzip._1.toList
    val className = classOf(e)
    if (!names.contains(Name("_plot", 0))) null
    else {
      e.foldLeft (List.empty[Name]) { (p: List[Name], o: (Name, GValue)) =>
        val name = o._1
        val value = o._2
        if (name.x == "_plot")
          value match {
            case VVector(l) =>
              if (l.nonEmpty && l.distinct.size != l.size)  // check whether there are duplicates in _plot
                throw DuplicatesForPlot(className.x)
              else if (l.nonEmpty) p ::: l.map( v => extractVariableName(v, names, className) )
              else p
            case VLit(GStr(v)) =>
              p :+ parseVariableName(v)
            case _ => throw UnsupportedPlotType(value.toString)
          }
        else p
      }
    }
  }
  /** Extract the variable from GValue to a Name */
  def extractVariableName(variable: Value[_], names: List[Name], objName: ClassName): Name = {
    variable match {
      case VLit(vs) =>
        val variableString = Conversions.extractString(vs)
        val variableName = parseVariableName(variableString)
        if (names.contains(variableName)) variableName  // check whether the variable name for plot is exist
        else throw NonexistentVariableForPlot(variableName.x, objName.x)
      case _ => throw UnsupportedPlotType(variable.toString)
    }
  }
  def parseVariableName(name: String): Name = {
    try
      Parser.run(Parser.name, name)
    catch {
      case e: Exception => throw UnsupportedPlotType(name)
    }
  }
  //def sortObject(e:GObject) = {
  //  e.toList.sortWith { (a,b) => a._1 < b._1 } 
  //}
}

class FilterRowsDataAdder(opts: CStoreOpts) extends FilterDataAdder(opts) {
  override def newStep(t: ResultType) = {super.newStep(t); ShouldAddData.No}
  def addData(objId: CId, values: GObject, tag: Tag = Tag.root, deadTag: Boolean = false) = {}
  def continue = !outputRow 
}

// Legacy DumpSample for 2014 and before
class LegacyDumpSample(out: java.io.PrintStream) extends DumpSample(out) {
   override val initialShouldAddData = ShouldAddData.IfLast
   override def addLast : Boolean = (shouldAddData == ShouldAddData.IfLast)
   prevStepType = Discrete
   curStepType = Discrete
   shouldAddData = ShouldAddData.IfLast
}

// Filter used for regression tests
class DumpSample(out: java.io.PrintStream) extends DataAdder {
  val pp = new Pretty
  pp.filterStore = true
  pp.predictableDoubles = true
  var stepNum = -1
  var discrStepNum = -1
  var last : CStore = null
  var prevStepType : ResultType = Initial
  var curStepType : ResultType = Initial
  var useResult : ShouldAddData.Value =  ShouldAddData.No
  var store = mutable.Map.empty[Tag, mutable.MutableList[(CId, GObject)]]
  
  def dumpLastStep = {
    val ordTags = store.keys.toList.sorted
    ordTags foreach (tag => out.println(pp.pprint(pp.prettyStore(store(tag)))))
    // FIXME: I strongly recommend to remove the next line once the tests have been done successfully and to regenerate the res files
    // It is there only to mimic the previous situation where the store was printed even if empty
    // (just a new line in that case), but it has no reason to continue to exist.
    if(store.isEmpty) out.println(pp.pprint(pp.prettyStore(new mutable.MutableList[(CId, GObject)])))
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
  def addData(objId: CId, values: GObject, tag: Tag = Tag.root, deadTag: Boolean = false) = {
    if(!store.contains(tag)) store += tag -> new mutable.MutableList[(CId, GObject)]
    store(tag) += ((objId, values.toList)) // need to make a copy of the values as they could change
  }
  def continue = true
}

//
/* ************************************************************************ */
//

case class CStoreRes(ctrace: Stream[CStore], metadata: Metadata) extends InterpreterRes {
  def print = {
    var i = 0
    for (st <- ctrace) {
      println(pprint(prettyStore(st)))
      println("-" * 30 + i)
      i += 1
    }
  }
  
  private def loop(action: (CStore, ResultType) => Unit) : Unit = {
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

