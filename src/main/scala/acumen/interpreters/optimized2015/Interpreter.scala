package acumen
package interpreters
package optimized2015

import scala.collection.immutable.HashMap

import acumen.Errors._
import acumen.Pretty._
import util.ASTUtil.checkContinuousAssignmentToSimulator
import acumen.util.Conversions._
import acumen.util.Random
import acumen.interpreters.Common._
import acumen.util.Canonical.{
  childrenOf, 
  classf,
  cmain,
  cmagic,
  deadStore,
  endTime,
  nextChild,
  parentOf,
  parent,
  seedOf,
  seed1,
  seed2,
  self,
  resultType,
  time,
  timeStep,
  setObject,
  setSeed
}
import scala.annotation.tailrec

class Interpreter extends CStoreInterpreter {

  import Common._ 

  val interpreterType = TraditionalInterpreterType
  type Store = Common.Store
  def repr (s:Store) : CStore = Common.repr(s)
  def fromCStore (cs:CStore, root:CId) : Store = Common.fromCStore(cs, root)
  override def isDead(cs: Interpreter.this.Store): Boolean = Common.isDead(cs)
  val initStepType = Initial
  val timeStep = 0.015625
  val outputRows = "All"
  val initStore = initStoreInterpreter(initStep = initStepType, initTimeStep = timeStep, initOutputRows = outputRows, isImperative = true, interpreterType = TraditionalInterpreterType)
  override def visibleParameters = visibleParametersMap(initStore) + ("method" -> VLit(GStr(RungeKutta))) + ("orderOfIntegration" -> VLit(GInt(4)))
  private var reachFixPoint = true
  
  def lift = identLift
  
  def init(prog: Prog): (Prog, SuperStore, SuperMetadata) = {
    checkContinuousAssignmentToSimulator(prog)
    val magic = fromCStore(initStore, CId(0))
    val cprog = CleanParameters.run(prog, TraditionalInterpreterType)
    val (sd1, sd2) = Random.split(Random.mkGen(0))
    val mainObj = mkObj(cmain, cprog, IsMain, sd1, List(VObjId(Some(magic))), magic, 1)
    magic.seed = sd2
    val mprog = Prog(magicClass :: cprog.defs)
    setVarNum(magic, countStateVars(repr(mainObj)))
    val splitStore = splitIntervalsStore(mainObj)
    val metadatas = splitStore mapValues (obj => {
      checkHypothesis(getSimulator(obj).phaseParms, mprog, getSimulator(obj), obj)
      getMetadata(obj)
    })
    (mprog , splitStore, metadatas)
  }

  // Hypotheses check
  def checkHypothesis(pp : PhaseParms, p: Prog, magic : Store, st: Object) {
      pp.reset(Ignore, Ignore, Ignore)
      pp.usePrev = false
      pp.doHypothesis = true
      magic.phaseParms.stepHypothesisResults = Map.empty
      traverse(evalStep(p, magic), st)
      val md = 
        if (magic.phaseParms.stepHypothesisResults nonEmpty) 
          SomeMetadata(magic.phaseParms.stepHypothesisResults,
                       magic.phaseParms.hypTimeDomainLeft, getTime(magic),
                       false, None)
        else NoMetadata
      magic.phaseParms.metaData = magic.phaseParms.metaData.combine(md)
    }
  
  def localStep(p: Prog, st: Store): (ResultType, SuperStore) = {
    val magic = getSimulator(st)
        
    // SuperStore returned at the end of LocalStep. Will be updated if some split are required
    var sst: SuperStore = Map((Tag.root, st))
    
    val pp = magic.phaseParms
    pp.curIter += 1

    /** If false then there exist active discrete assignments or non-clashing
      * continuous assignments */
    var isFixedPoint : Boolean = true

    def assignmentInspection(v: Val) = v match {
      case VLit(GInterval(_)) => pp.splitMe = true
      case _ =>
    }

    /** Retrieves the collected discrete assignments */
    def doEquationD() = try {
      var idx = 0
      while (idx < pp.das.size) {
        val da = pp.das(idx)
        // Execute the assignment and check for duplicates
        setField(da.id, da.field, da.v, da.pos) match {
          case NoChange() =>
          case SomeChange(_,_) =>
            isFixedPoint = false
            reachFixPoint = false
        }
        //If the previously set value is prohibited (Inf or Nan, set the Store dead)
        //If a splitInterval is assigned, trigger the splitMe flag
        assignmentInspection(da.v)
        idx += 1
      }
    }

    /** Filters the collected continuous assignments */
    def filterEquationT() = try {

      var idx = 0
      while (idx < pp.assigns.size) {
        val (eqt, pos) = pp.assigns(idx)
        
        // The continuous assignment is non-clashing
        if (!magic.phaseParms.das.exists {
          a => a.id == eqt.id && a.field == eqt.field
        }) {
          // Initialize the field:
          // mark the value to be updated and check for duplicates
          setField(eqt.id, eqt.field, AssignLookup(idx), pos)
          idx += 1
        
        // The assignment clashes with some discrete assignment
        } else {
          pp.assigns.remove(idx)
        }
      }
    }
    
    /** Retrieves the collected continuous assignments */
    def doEquationT(odeEnv: OdeEnv) = try {
      
      var idx = 0
      pp.usePrev = false
      
      while (idx < pp.assigns.size) {
        val (eqt, _) = pp.assigns(idx)
        // Algebraic loop is checked
        val v = getField(eqt.id, eqt.field, p, Env(eqt.env,Some(odeEnv)))
        // Execute the assignment
        updateField(eqt.id, eqt.field, v) match {
          case NoChange() =>
          case SomeChange(_,_) =>
            isFixedPoint = false
            reachFixPoint = false
        }
        //If the previously set value is prohibited (Inf or Nan, set the Store dead)
        //If a splitInterval is assigned, trigger the splitMe flag
        assignmentInspection(v)
        idx += 1
      }
    } finally {
      pp.usePrev = true
    }

    if (getResultType(magic) == FixedPoint && getTime(magic) >= getEndTime(magic)) {

      (null, sst)

    } else {

      val rt = if (getResultType(magic) != FixedPoint) { // Discrete Step

        // Gather discrete assignments, structural actions are ignored
        pp.reset(Gather, Preserve, Ignore)
        traverse(evalStep(p, magic), st) 

        // Gather all continuous assignments.
        pp.reset(Preserve, Gather, Ignore)
        traverse(evalStep(p, magic), st)
        
        // Execute creates, other structural actions are ignored.
        // Append the list of discrete assignments.
        pp.reset(CreateOnly, Preserve, Ignore)
        traverse(evalStep(p, magic), st) 

        // Collect structural actions.
        pp.reset(Structural, Preserve, Ignore)
        val reParentings = traverse(evalStep(p, magic), st) 

        // Retrieve updated values for discrete assignments.
        doEquationD()
        
        // Mark that we begin processing the continuous assignments.
        pp.curIter += 1

        // Filter out clashing continuous assignments.
        filterEquationT()
        
        // Retrieve updated values the non-clashing ones.
        doEquationT(OdeEnv(IndexedSeq.empty, Array.fill[AssignVal](pp.assigns.length)(Unknown), magic))

        // Apply the structural actions: elim, move.
        reParentings match {
          case SomeChange(dead, rps) =>
            for ((o, p) <- rps)
              changeParent(o, p)
            for (o <- dead) {
              o.parent match {
                case None => ()
                case Some(op) =>
                  for (oc <- o.children) changeParent(oc, op)
                  op.children = op.children diff Seq(o)
              }
            }
            isFixedPoint = false
        
          case NoChange() =>
        }
        
        // Decide if a FixedPoint is reached
        if (isFixedPoint) { 
          FixedPoint
        } else {
          pp.hypTimeDomainLeft = getTime(magic)

          Discrete
        } 
      } else { // Continuous step

        // Gather continuous assignments and integrations
        pp.reset(Ignore, Gather, Gather)
        traverse(evalStep(p, magic), st)

        // After the pp.reset the das list is empty,
        // thus filtering only initializes the fields
        filterEquationT()
        
        checkContinuousDynamicsAlwaysDefined(st, magic)
        
        // Compute the initial values the the ode solver
        val sz = pp.odes.size
        val initVal = new Array[Val](sz)
        var idx = 0
        while (idx < sz) {
          val eqt = pp.odes(idx)
          val vv = eqt.id.fields(eqt.field)
          assert(pp.curIter == vv.lastUpdated)
          initVal(idx) = eqt.id.fields(eqt.field).prevSetVal
          idx += 1
        }

        // Run the ode solver
        implicit val field = FieldImpl(pp.odes, p)
        val initOdeEnv = OdeEnv(initVal, Array.fill[AssignVal](pp.assigns.length)(Unknown), magic)
        val res = new Solver[ObjId,OdeEnv,Double](getField(magic, Name("method", 0)), initOdeEnv, getTimeStep(magic)).solve

        // Evaluate (if necessary) and update values assigned to by EquationT
        doEquationT(res)

        // Update the fields based on the result from the ode solver
        idx = 0
        while (idx < sz) {
          val eqt = pp.odes(idx)
          val value: Val = res.odeVals(idx)
          updateField(eqt.id, eqt.field, value)
          //If the previously set value is prohibited (Inf or Nan, set the Store dead)
          //If a splitInterval is assigned, trigger the splitMe flag
          assignmentInspection(value)
          idx += 1
        }

        pp.hypTimeDomainLeft = getTime(magic)
        setTime(magic, getTime(magic) + getTimeStep(magic))
        
        Continuous
      }

      setResultType(magic, rt)
      setVarNum(magic, countStateVars(repr(st)))
      val reprSt = repr(st)
      reprSt.foldLeft(reprSt){
        case (res1, (id,o)) =>
        val (seed1,seed2) = getNewSeed(st)
        val (tmp,newSeed) = Random.next((seed1,seed2))
        setObject(id, setSeed(reprSt(id), newSeed), res1)
      }
      
      if (pp.splitMe) {
        sst = splitIntervalsStore(st)
        sst foreach {case (_, st) => getSimulator(st).phaseParms.splitMe = false}
      }
      if (rt != FixedPoint) checkHypothesis(pp, p, magic, st)
      (rt, sst)
    }
  }


  def step(p: Prog, st: Store, md: Metadata): StepRes = {
    setMetadata(st, md)
    val (resultType, sst) = localStep(p, st)
    if (resultType == null) Done(Map(Tag.root -> md), getEndTime(getSimulator(st)))
    else Data(sst, sst mapValues getMetadata)
  }

  // always returns the last known step, the adder callback is used to
  // determine when teh simulation is done
  override def multiStep(p: Prog, st: Store, md: Metadata, adder: DataAdder, baseTag: Tag): Map[Tag, (Store, Metadata, Double)] = {
    setMetadata(st, md)
    adder.shouldAddData = adder.initialShouldAddData
    var (res, sst): (ResultType, SuperStore) = (null, Map.empty)
    var endTimes = Map.empty[Tag, Double]
    @tailrec def step0() : Unit = {
      localStep(p, st) match {
        case (r, s) => res = r; sst = s
      }
      val magics = sst mapValues getSimulator
      if (res == null) {
        if (adder.addLast)
          sst foreach { case (t, st) => addData(st, adder, t :: baseTag, isDead(st)) }
        adder.noMoreData()
        endTimes = magics mapValues getEndTime
      } else {
        sst foreach { case (t, st) =>
          endTimes += t -> Double.NaN
          adder.shouldAddData = adder.newStep(res)
          if (adder.shouldAddData == ShouldAddData.Yes)
            addData(st, adder, t :: baseTag, isDead(st))
        }
        // It is impossible to continue if a split occur during the last step. If no split occur, then sst.values == st.
        if (adder.continue && sst.size == 1)
          step0()
        else if (adder.continue) throw ShouldNeverHappen()
      }
    }
    step0()
    sst map { case (t, st) => (t::baseTag) -> (st, getMetadata(st), endTimes(t)) }
  }

  def addData(st: Store, adder: DataAdder, tag: Tag, deadTag: Boolean) : Unit = {
    // Note: Conversion to a CStore just to add the data is certainly
    // not the most efficient way to go about things, but for now it
    // will do. --kevina
    adder.addData(st.id, st.fieldsCur, tag, deadTag)
    st.children.foreach { child => addData(child, adder, tag, deadTag) }
  }
}

object Interpreter extends Interpreter()
