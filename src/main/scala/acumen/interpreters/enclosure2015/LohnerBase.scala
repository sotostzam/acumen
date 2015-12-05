package acumen
package interpreters
package enclosure2015

import scala.annotation.tailrec
import enclosure.Interval
import enclosure2015.Common._
import interpreters.Common._
import FAD.FDif
import TAD.TDifAsReal
import Pretty.pprint
import util._
import util.Canonical._
import util.Conversions.{
  extractDouble, extractDoubles, extractId, extractInterval, extractIntervals
}
import Errors._

/** Solver based on Taylor series integration. */
object LohnerEnclosureSolver extends EnclosureSolver[DynSetEnclosure] {
  
  def convertEnclosure(e: Enclosure): DynSetEnclosure = e match {
    case dse: DynSetEnclosure => dse
    case _ => intervalBase initializeEnclosure e.cStore 
  }
  
  def continuousEncloser
    ( odes: Set[CollectedAction]
    , eqs: Set[CollectedAction]
    , claims: Set[CollectedConstraint]
    , T: Interval
    , p: Prog
    , enc: DynSetEnclosure
    , evalExpr: (Expr, Env, EStore) => CValue
    ): (Enclosure, Enclosure) = {
    Logger.trace(s"continuousEncloserLohner (over $T)")
    
    val field = inline(eqs, odes, enc.cStore) // in-line eqs to obtain explicit ODEs

    val flowVariables = enc.indexToName.values.map{ case (id,n) => QName(id,n) }.toList // used in jacPhi for lifting
    
    // TODO the class uses odeVariables and enc from the outside
    case class TaylorIntegrator( odeList             : List[CollectedAction]
                               , timeStep            : Interval
                               , orderOfIntegration  : Int 
                               , maxPicardIterations : Int 
                               ) extends C1Flow {
      
      val timeStepInterval = Interval(0, timeStep.hiDouble)
          
      /** Computes the apriori enclosure (a coarse range enclosure of the flow) */ 
      def apply(x: RealVector): RealVector = {
        implicit val useIntervalArithmetic: Real[CValue] = intervalBase.cValueIsReal
        implicit val intervalField = intervalBase.FieldImpl(odeList, evalExpr)
        val step: CValue = VLit(GConstantRealEnclosure(timeStepInterval))
        @tailrec def picardIterator(candidate: RealVector, iterations: Int): RealVector = {
           val fieldAppliedToCandidate = intervalField(DynSetEnclosure(candidate, enc))
           val c = x + step ** fieldAppliedToCandidate.dynSet
           val invalidEnclosureDirections = (0 until enc.dim).filterNot(i => (candidate(i), c(i)) match {
             case (VLit(GConstantRealEnclosure(e)), VLit(GConstantRealEnclosure(ce))) => 
               e containsInInterior ce 
             })
           lazy val candidateNext: RealVector = breeze.linalg.Vector.tabulate(enc.dim){ i =>
             if (invalidEnclosureDirections contains i) {
               val VLit(GConstantRealEnclosure(e)) = c(i)
               val m = Interval(e.midpoint).hiDouble
               val wHalf = (e.width.hiDouble * 1.5) / 2
               VLit(GConstantRealEnclosure(Interval(m - wHalf, m + wHalf)))
             } else candidate(i)
           } 
           if (iterations > maxPicardIterations) sys.error(s"Unable to find valid enclosure over $T in $maxPicardIterations iterations.")
           if (invalidEnclosureDirections isEmpty) {
             Logger.debug(s"apriori enclosure over $T has been generated in $iterations iteration(s)")
             candidate 
           } else 
             picardIterator(candidateNext, iterations + 1)
        }
        val fieldAppliedToLohnerSet = intervalField(DynSetEnclosure(x, enc))
        val candidateStep: CValue = VLit(GConstantRealEnclosure(Interval(-0.2, 1.2) * timeStep))
        val epsilon: RealVector = breeze.linalg.Vector.tabulate(enc.dim){ i => VLit(GConstantRealEnclosure(Interval(-1, 1) * 1e-21)) }
        picardIterator(x + candidateStep ** fieldAppliedToLohnerSet.dynSet + epsilon, 0)
      }
      
      /** Computes the finite Taylor expansion */
      def phi(x: RealVector, timeStep: Interval): RealVector = {
        implicit val useIntervalArithmetic: Real[CValue] = intervalBase.cValueIsReal
        implicit val intervalField = intervalBase.FieldImpl(odeList, evalExpr)
        implicit def liftToRichStore(s: DynSetEnclosure): intervalBase.RichStoreImpl = intervalBase.liftDynSetEnclosure(s) // linearTransformationField passed implicitly
        val xLift = DynSetEnclosure(x, enc) // FIXME was enc.midpoint
        val imageOfx = solveIVPTaylor[CId,DynSetEnclosure,CValue](xLift, VLit(GConstantRealEnclosure(timeStep)), orderOfIntegration)
        imageOfx.dynSet
      }
      
      /** Computes the Jacobian of phi */
      def jacPhi(x: RealVector, timeStep: Interval): RealMatrix = {
        implicit val useFDifArithmetic: Real[CValue] = fDifBase.cValueIsReal
        implicit val linearTransformationField = fDifBase.FieldImpl(odeList.map(_ mapRhs (FAD.lift[CId,CValue](_, flowVariables))), evalExpr)
        implicit def liftToRichStore(s: DynSetEnclosure): fDifBase.RichStoreImpl = fDifBase.liftDynSetEnclosure(s) // linearTransformationField passed implicitly
        val p = FAD.lift[CId,DynSetEnclosure,CValue](DynSetEnclosure(x, enc), flowVariables) // FIXME parameter2 was enc.outerEnclosure
        val myStepWrapped = {
          val VLit(GIntervalFDif(FAD.FDif(_, zeroCoeffs))) = useFDifArithmetic.fromDouble(0)
          VLit(GIntervalFDif(FAD.FDif(timeStep, zeroCoeffs)))
        }
        val linearTransformationSolution = // linearTransformationField passed implicitly
        solveIVPTaylor[CId,DynSetEnclosure,CValue](p, myStepWrapped, orderOfIntegration)
        breeze.linalg.Matrix.tabulate[CValue](enc.dim, enc.dim) {
          case (r, c) =>
            (linearTransformationSolution dynSet c) match {
              case VLit(GIntervalFDif(FAD.FDif(_, ds))) =>
                val (id, n) = linearTransformationSolution indexToName r
                VLit(GConstantRealEnclosure(ds(QName(id, n))))
            }
        }
      }
      
      /** Bounds the difference between the finite Taylor expansion and the flow */
      def remainder(x: RealVector, timeStep: Interval): RealVector = {
        implicit val useIntervalArithmetic: Real[CValue] = intervalBase.cValueIsReal
        implicit val intervalField = intervalBase.FieldImpl(odeList, evalExpr)
        implicit def liftToRichStore(s: DynSetEnclosure): intervalBase.RichStoreImpl = intervalBase.liftDynSetEnclosure(s) // linearTransformationField passed implicitly
        val p = DynSetEnclosure(x, enc)
        val tcs = computeTaylorCoefficients[CId,DynSetEnclosure,CValue](p, orderOfIntegration + 1)
        val factor = VLit(GConstantRealEnclosure(timeStep pow (orderOfIntegration + 1)))
        // TODO remove outerEnclosure
        tcs.dynSet.outerEnclosure.map(_ match { case VLit(GCValueTDif(tdif)) => (tdif coeff (orderOfIntegration + 1)) * factor })
      }
      
      /* C1Flow interface */
      
      // computation of the image
      def       phi(x: RealVector) =       phi(x, timeStep)
      def    jacPhi(x: RealVector) =    jacPhi(x, timeStep)
      def remainder(x: RealVector) = remainder(x, timeStep)
     
      // computation of the range
      val self  = this
      val range = new C1Mapping {
        def     apply(x: RealVector) : RealVector = self     apply(x)
        def       phi(x: RealVector) : RealVector = self       phi(x, timeStepInterval)
        def    jacPhi(x: RealVector) : RealMatrix = self    jacPhi(x, timeStepInterval)
        def remainder(x: RealVector) : RealVector = self remainder(x, timeStepInterval)
      }
    
    }    

    val myIntegrator = TaylorIntegrator(field.toList,T.width, Common orderOfIntegration enc.cStore, Common maxPicardIterations enc.cStore)
    
    val eqsInlined = inline(eqs, eqs, enc.cStore) // LHSs of EquationsTs, including highest derivatives of ODEs
    
    // TODO Align field.toList with the order in enc.indexToName
    enc.move(eqsInlined, myIntegrator, evalExpr)

  }
  
}

/** Interval evaluation of solveIVPTaylor */
object intervalBase extends LohnerBase()(intervalCValueIsReal, intervalCValueTDifIsReal)
  
/** FDif[Interval] evaluation of solveIVPTaylor */
object fDifBase extends LohnerBase()(fDifCValueIsReal, fDifCValueTDifIsReal)

/** Base for evaluation of solveIVPTaylor */
case class LohnerBase
  ( implicit cValueIsReal:     Real[CValue]
  ,          cValueTDifIsReal: TDifAsReal[CValue] 
  ) extends SolverBase {
  
  /* SolverBase interface */
  type E = DynSetEnclosure
  def initializeEnclosure(st: CStore) = DynSetEnclosure(st)
  def solver = LohnerEnclosureSolver 
  
  /* Implicit Conversions */
  implicit class RichStoreImpl(dynSetEnclosure: DynSetEnclosure) extends RichStore[DynSetEnclosure,CId] {
    /* RichStore */
    override def +++(that: DynSetEnclosure): DynSetEnclosure =
      dynSetEnclosure.copy(dynSet = IntervalBox(breeze.linalg.Vector.tabulate(dynSetEnclosure.dim)(i => dynSetEnclosure.dynSet(i) + that.dynSet(i))))
    override def ***(that: Double): DynSetEnclosure =
      dynSetEnclosure.copy(dynSet = IntervalBox(breeze.linalg.Vector.tabulate(dynSetEnclosure.dim)(i => dynSetEnclosure.dynSet(i) * cValueIsReal.fromDouble(that))))
    override def map(m: CValue => CValue): DynSetEnclosure = dynSetEnclosure.copy(dynSet = dynSetEnclosure.dynSet map m)
    override def mapName(m: (GId, Name, CValue) => CValue): DynSetEnclosure =
      dynSetEnclosure.copy(dynSet = IntervalBox(breeze.linalg.Vector.tabulate[CValue](dynSetEnclosure.dim){ i => 
        val (cid,n) = dynSetEnclosure indexToName i
        m(cid, n, dynSetEnclosure dynSet i)
      }))
    override def apply(id: CId, n: Name): CValue = dynSetEnclosure.dynSet(dynSetEnclosure.nameToIndex(id, n))
    override def updated(id: CId, n: Name, v: CValue): DynSetEnclosure =
      // TODO: Group updates or do this with mutation instead
      dynSetEnclosure.copy(dynSet = { val encl = dynSetEnclosure.dynSet.copy; encl.update(dynSetEnclosure.nameToIndex(id, n), v); IntervalBox(encl) })
    override def getInSimulator(variable: String) = dynSetEnclosure.getInSimulator(variable)
  }
  
  def liftDynSetEnclosure(s: DynSetEnclosure)(implicit field: FieldImpl): RichStoreImpl = RichStoreImpl(s)
  
  case class FieldImpl(odes: List[CollectedAction], evalExpr: (Expr, Env, EStore) => CValue) extends interpreters.Common.Field[DynSetEnclosure,CId] {
    override def apply(dynSetEnclosure: DynSetEnclosure): DynSetEnclosure = {
      val s1 = dynSetEnclosure.dynSet.copy
      odes.foreach{ ode => 
        s1.update(dynSetEnclosure.nameToIndex(ode.lhs.id, ode.lhs.field), evalExpr(ode.rhs, ode.env, dynSetEnclosure)) 
      }
      dynSetEnclosure.copy(dynSet = IntervalBox(s1))
    }
    override def variables(s: DynSetEnclosure): List[(CId, Name)] = odes.map(ode => (ode.lhs.id, ode.lhs.field))
    override def map(em: Expr => Expr) =
      FieldImpl(odes.map(ode => ode.copy(a = (ode.a: @unchecked) match {
        case Discretely(Assign(lhs: Expr, rhs: Expr)) =>
          Discretely(Assign(em(lhs), em(rhs)))
        case Continuously(EquationT(lhs: Expr, rhs: Expr)) =>
          Continuously(EquationT(em(lhs), em(rhs)))
        case Continuously(EquationI(lhs: Expr, rhs: Expr)) =>
          Continuously(EquationI(em(lhs), em(rhs)))
      })), evalExpr)
  }
  
}

