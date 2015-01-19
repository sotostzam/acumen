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

class Interpreter(val specialInitContStep : Boolean = false) extends CStoreInterpreter {

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
    if (specialInitContStep)
      println("Doing specialInitContStep")
    if (specialInitContStep)
      magic.phaseParms.specialInitialStep = true
    (mprog , mainObj, NoMetadata)
  }

  def localStep(p: Prog, st: Store): ResultType = {
    val magic = getSimulator(st)
  
    val pp = magic.phaseParms
    pp.curIter += 1

    def doEquationT(odeEnv: OdeEnv) = try {
      pp.usePrev = false
      var idx = 0
      while (idx < pp.assigns.size) {
        val eqt = pp.assigns(idx)
        val v = getField(eqt.id, eqt.field, p, Env(eqt.env,Some(odeEnv)))
        updateField(eqt.id, eqt.field, v)
        idx += 1
      }
    } finally {
      pp.usePrev = true
    }

    def checkHypothesis() {
      pp.reset(false, Ignore, Ignore)
      pp.usePrev = false
      pp.doHypothesis = true
      traverse(evalStep(p, magic), st)
    }

    if (getResultType(magic) == FixedPoint && getTime(magic) >= getEndTime(magic)) {

      null

    } else {

      val rt = if (getResultType(magic) != FixedPoint) { // Discrete Step

        pp.reset(true, Ignore, Ignore)

        traverse(evalStep(p, magic), st) match {
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

            // gather the continuous assignments 
            pp.curIter += 1
            pp.reset(false, Gather, Ignore)
            traverse(evalStep(p, magic), st)
            // Retrieve updated values for continuous assignments 
            doEquationT(OdeEnv(IndexedSeq.empty, Array.fill[AssignVal](pp.assigns.length)(Unknown)))

            checkHypothesis()

            Discrete
          case NoChange() =>
            FixedPoint
          }

      } else { // Continuous step

        pp.reset(false, Gather, Gather)

        traverse(evalStep(p, magic), st)

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
        val res = if (pp.specialInitialStep) initOdeEnv
                  else new Solver(getField(magic, Name("method", 0)), initOdeEnv, getTimeStep(magic)).solve

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

        if (pp.specialInitialStep)
          pp.specialInitialStep = false
        else
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
    var shouldAddData = ShouldAddData.IfLast
    // ^^ set to IfLast on purpose to make things work
    var endTime = Double.NaN
    @tailrec def step0() : Unit = {
      val res = localStep(p, st)
      if (res == null) {
        if (shouldAddData == ShouldAddData.IfLast)
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

object Interpreter extends Interpreter(false)

