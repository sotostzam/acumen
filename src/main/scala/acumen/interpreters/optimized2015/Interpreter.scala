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
  timeStep
}
import scala.annotation.tailrec

class Interpreter extends CStoreInterpreter {

  import Common._ 

  type Store = Common.Store
  def repr (s:Store) : CStore = Common.repr(s)
  def fromCStore (cs:CStore, root:CId) : Store = Common.fromCStore(cs, root)
  override def visibleParameters = visibleParametersImpr

  def init(prog: Prog): (Prog, Store, Metadata) = {
    checkContinuousAssignmentToSimulator(prog)
    val magic = fromCStore(initStoreImpr, CId(0))
    val cprog = CleanParameters.run(prog, CStoreInterpreterType)
    val sprog = Simplifier.run(cprog)
    val (sd1, sd2) = Random.split(Random.mkGen(0))
    val mainObj = mkObj(cmain, sprog, IsMain, sd1, List(VObjId(Some(magic))), magic, 1)
    magic.seed = sd2
    val mprog = Prog(magicClass :: sprog.defs)
    (mprog , mainObj, NoMetadata)
  }

  def localStep(p: Prog, st: Store): ResultType = {
    val magic = getSimulator(st)
  
    val pp = magic.phaseParms
    pp.curIter += 1

    /** If false then there exist active discrete assignments or non-clashing
      * continuous assignments */
    var isFixedPoint : Boolean = true

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
        }
        idx += 1
      }
    }

    /** Filters the collected continuous assignments */
    def filterEquationT() = try {

      var idx = 0
      while (idx < pp.assigns.size) {
        val (eqt, pos) = pp.assigns(idx)
        
        // The continuous assignment is non-clashing
        if (( magic.phaseParms.das filter 
               (d => d match { case Assignment(eqt.id, eqt.field, _, _) => true
                              case _                                   => false }) ).isEmpty) {
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
        }
        idx += 1
      }
    } finally {
      pp.usePrev = true
    }

    def checkHypothesis() {
      pp.reset(Ignore, Ignore, Ignore)
      pp.usePrev = false
      pp.doHypothesis = true
      traverse(evalStep(p, magic), st)
    }

    if (getResultType(magic) == FixedPoint && getTime(magic) >= getEndTime(magic)) {

      null

    } else {

      val rt = if (getResultType(magic) != FixedPoint) { // Discrete Step

        // Gather discrete assignments, creates are ignored
        // Collect structural actions.
        pp.reset(Gather, Preserve, Ignore)
        val reParentings = traverse(evalStep(p, magic), st) 

        // Gather all continuous assignments.
        pp.reset(Preserve, Gather, Ignore)
        traverse(evalStep(p, magic), st)
        
        // Execute creates.
        // Append the list of discrete assignments.
        pp.reset(CreateOnly, Preserve, Ignore)
        traverse(evalStep(p, magic), st) 

        // Retrieve updated values for discrete assignments.
        doEquationD()
        
        // Mark that we begin processing the continuous assignments.
        pp.curIter += 1

        // Filter out clashing continuous assignments.
        filterEquationT()
        
        // Retrieve updated values the non-clashing ones.
        doEquationT(OdeEnv(IndexedSeq.empty, Array.fill[AssignVal](pp.assigns.length)(Unknown)))

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
          checkHypothesis()

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
        val initOdeEnv = OdeEnv(initVal, Array.fill[AssignVal](pp.assigns.length)(Unknown))
        val res = new Solver(getField(magic, Name("method", 0)), initOdeEnv, getTimeStep(magic)).solve

        // Evaluate (if necessary) and update values assigned to by EquationT
        doEquationT(res)

        // Update the fields based on the result from the ode solver
        idx = 0
        while (idx < sz) {
          val eqt = pp.odes(idx)
          updateField(eqt.id, eqt.field, res.odeVals(idx))
          idx += 1
        }

        checkHypothesis()

        setTime(magic, getTime(magic) + getTimeStep(magic))

        Continuous
      }

      setResultType(magic, rt)

      rt
    }
  }

  def step(p: Prog, st: Store, md: Metadata): StepRes = {
    setMetadata(st, md)
    val res = localStep(p, st)
    if (res == null) Done(md, getEndTime(getSimulator(st)))
    else Data(st,getMetadata(st)) 
  }

  // always returns the last known step, the adder callback is used to
  // determine when teh simulation is done
  override def multiStep(p: Prog, st: Store, md: Metadata, adder: DataAdder): (Store, Metadata, Double) = {
    setMetadata(st, md)
    var shouldAddData = ShouldAddData.No
    var endTime = Double.NaN
    @tailrec def step0() : Unit = {
      val res = localStep(p, st)
      if (res == null) {
        if (adder.addLast)
          addData(st, adder)
        adder.noMoreData()
        endTime = getEndTime(getSimulator(st))
      } else {
        shouldAddData = adder.newStep(res)
        if (shouldAddData == ShouldAddData.Yes)
          addData(st, adder)
        if (adder.continue)
          step0()
      }
    }
    step0()
    (st, getMetadata(st), endTime) 
  }

  def addData(st: Store, adder: DataAdder) : Unit = {
    // Note: Conversion to a CStore just to add the data is certainly
    // not the most efficient way to go about things, but for now it
    // will do. --kevina
    adder.addData(st.id, st.fieldsCur)
    st.children.foreach { child => addData(child, adder) }
  }
}

object Interpreter extends Interpreter()
