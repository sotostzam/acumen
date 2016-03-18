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
    , encIn: DynSetEnclosure
    , evalExpr: (Expr, Env, EStore) => CValue
    )( implicit parameters: Parameters
    ): (Enclosure, Enclosure) = {
    Logger.trace(s"continuousEncloserLohner (over $T)")

    // in-line eqs to obtain explicit ODEs
    val field = inline(eqs, odes, encIn.cStore)
    // LHSs of EquationsTs, including highest derivatives of ODEs
    val eqsInlined = inline(eqs, eqs, encIn.cStore) 

    // There are no ODEs in scope
    if (field.isEmpty) {
      Logger.trace("continuousEncloserLohner: there are no ODEs in scope")
      // There are no EquationTs in scope
      if (eqsInlined.isEmpty) {
        Logger.trace("continuousEncloserLohner: there are no EqTs in scope")
        (encIn, encIn)
      // There are some EquationTs in scope
      } else {
        Logger.trace("continuousEncloserLohner: there are some EqTs in scope")
        val equationTsApplied = 
          eqsInlined.foldLeft(encIn: DynSetEnclosure){ case (stTmp, ca) =>
            val rd = ca.lhs
            val cv = evalExpr(ca.rhs, ca.env, encIn)
            stTmp.setObjectField(rd.id, rd.field, cv)
          }
        (equationTsApplied, equationTsApplied)
      }
    } else {
    
    val nameToIndexNext = 
      DynSetEnclosure.buildNameToIndexMap(odes.map{ ca => val rd = ca.lhs; (rd.id, rd.field) })
      
    // FIXME Be more clever here, analogously to the issue with setting values in the IntervalDynSet
    val enc =
      if (nameToIndexNext == encIn.nameToIndex) encIn
      else // convert dynSet in enc to be of the dimension specified by odes 
        DynSetEnclosure(encIn.cStore, nameToIndexNext)(intervalBase cValueIsReal, parameters) 
    
    // TODO the class uses odeVariables and enc from the outside
    case class TaylorIntegrator( odeList             : List[CollectedAction]
                               , timeStep            : Interval
                               , orderOfIntegration  : Int 
                               , maxPicardIterations : Int
                               )( implicit parameters: Parameters
                               ) extends C1Flow {
      
      val timeStepInterval = Interval(0, timeStep.hiDouble)
          
      /** Computes the apriori enclosure (a coarse range enclosure of the flow) */ 
      def apply(x: RealVector)(implicit parameters: Parameters): RealVector = {
        implicit val useIntervalArithmetic: Real[CValue] = intervalBase.cValueIsReal
        implicit val intervalField = intervalBase.FieldImpl(odeList, eqsInlined, evalExpr)
        val step: CValue = VLit(GConstantRealEnclosure(timeStepInterval))
        @tailrec def picardIterator(candidate: RealVector, iterations: Int): RealVector = {
           val fieldAppliedToCandidate = intervalField(DynSetEnclosure(candidate, enc, eqsInlined, evalExpr))
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
             Logger.trace(s"apriori enclosure over $T has been generated in $iterations iteration(s)")
             candidate 
           } else 
             picardIterator(candidateNext, iterations + 1)
        }
        val fieldAppliedToLohnerSet = intervalField(DynSetEnclosure(x, enc, eqsInlined, evalExpr))
        val candidateStep: CValue = VLit(GConstantRealEnclosure(Interval(-0.2, 1.2) * timeStep))
        val epsilon: RealVector = breeze.linalg.Vector.tabulate(enc.dim){ i => VLit(GConstantRealEnclosure(Interval(-1, 1) * 1e-21)) }
        picardIterator(x + candidateStep ** fieldAppliedToLohnerSet.dynSet + epsilon, 0)
      }
      
      /** Computes the finite Taylor expansion */
      def phi(x: RealVector, timeStep: Interval)(implicit parameters: Parameters): RealVector = {
        implicit val useIntervalArithmetic: Real[CValue] = intervalBase.cValueIsReal
        implicit val intervalField = intervalBase.FieldImpl(odeList, Set.empty[CollectedAction], evalExpr)
        val xLift = DynSetEnclosure(x, enc, Set.empty[CollectedAction], evalExpr) // FIXME was enc.midpoint
        val imageOfx = solveIVPTaylor[CId,DynSetEnclosure,CValue](xLift, VLit(GConstantRealEnclosure(timeStep)), orderOfIntegration)
        imageOfx.dynSet
      }
      
      /** Computes the Jacobian of phi */
      def jacPhi(x: RealVector, timeStep: Interval)(implicit parameters: Parameters): RealMatrix = {
        implicit val useFDifArithmetic: Real[CValue] = fDifBase.cValueIsReal
        val flowVariables = enc.indexToName.values.map{ case (id,n) => QName(id,n) }.toList // used for lifting
        implicit val linearTransformationField = fDifBase.FieldImpl(odeList.map(_ mapRhs (FAD.lift[CId,CValue](_, flowVariables))), Set.empty[CollectedAction], evalExpr)
        val p = FAD.lift[CId,DynSetEnclosure,CValue](DynSetEnclosure(x, enc, Set.empty[CollectedAction], evalExpr), flowVariables) // FIXME parameter2 was enc.outerEnclosure
        val myStepWrapped = {
          val VLit(GIntervalFDif(FAD.FDif(_, zeroCoeffs))) = useFDifArithmetic.fromDouble(0)
          VLit(GIntervalFDif(FAD.FDif(timeStep, zeroCoeffs)))
        }
        val linearTransformationSolution = // linearTransformationField passed implicitly
        solveIVPTaylor[CId,DynSetEnclosure,CValue](p, myStepWrapped, orderOfIntegration)
        breeze.linalg.Matrix.tabulate[CValue](enc.dim, enc.dim) {
          case (r, c) =>
            (linearTransformationSolution dynSet r) match {
              case VLit(GIntervalFDif(FAD.FDif(_, ds))) =>
                val (id, n) = linearTransformationSolution indexToName c
                VLit(GConstantRealEnclosure(ds(QName(id, n))))
            }
        }
      }
      
      /** Bounds the difference between the finite Taylor expansion and the flow */
      def remainder(x: RealVector, timeStep: Interval)(implicit parameters: Parameters): RealVector = {
        implicit val useIntervalArithmetic: Real[CValue] = intervalBase.cValueIsReal
        implicit val intervalField = intervalBase.FieldImpl(odeList, Set.empty[CollectedAction], evalExpr)
        val p = DynSetEnclosure(x, enc, Set.empty[CollectedAction], evalExpr)
        val tcs = computeTaylorCoefficients[CId,DynSetEnclosure,CValue](p, orderOfIntegration + 1)
        val factor = VLit(GConstantRealEnclosure(timeStep pow (orderOfIntegration + 1)))
        // TODO remove outerEnclosure
        tcs.dynSet.outerEnclosure.map(_ match { case VLit(GCValueTDif(tdif)) => (tdif coeff (orderOfIntegration + 1)) * factor })
      }
      
      /* C1Flow interface */
      
      // computation of the image
      def       phi(x: RealVector)(implicit parameters: Parameters) =       phi(x, timeStep)
      def    jacPhi(x: RealVector)(implicit parameters: Parameters) =    jacPhi(x, timeStep)
      def remainder(x: RealVector)(implicit parameters: Parameters) = remainder(x, timeStep)
     
      // computation of the range
      val self  = this
      val range = new C1Mapping {
        def     apply(x: RealVector)(implicit parameters: Parameters) : RealVector = self     apply(x)
        def       phi(x: RealVector)(implicit parameters: Parameters) : RealVector = self       phi(x, timeStepInterval)
        def    jacPhi(x: RealVector)(implicit parameters: Parameters) : RealMatrix = self    jacPhi(x, timeStepInterval)
        def remainder(x: RealVector)(implicit parameters: Parameters) : RealVector = self remainder(x, timeStepInterval)
      }
    
    }    

    val myIntegrator = TaylorIntegrator(field.toList, T.width, parameters.orderOfIntegration, parameters.maxPicardIterations)
    
    // TODO Align field.toList with the order in enc.indexToName
    enc.move(eqsInlined, myIntegrator, evalExpr)

  }
  }
  
  def jumpEncloser
    ( das: Set[CollectedAction]
    , T: Interval
    , p: Prog
    , encIn: DynSetEnclosure
    , evalExpr: (Expr, Env, EStore) => CValue
    )( implicit parameters: Parameters
    ): Enclosure = {
    Logger.trace(s"jumpEncloserLohner (over $T)")

    val nameToIndexNext = 
      DynSetEnclosure.buildNameToIndexMap(das.map{ ca => val rd = ca.lhs; (rd.id, rd.field) })
      
    // FIXME Be more clever here, analogously to the issue with setting values in the IntervalDynSet
    val enc =
      if (nameToIndexNext == encIn.nameToIndex) encIn
      else // convert dynSet in enc to be of the dimension specified by odes 
        DynSetEnclosure(encIn.cStore, nameToIndexNext)(intervalBase cValueIsReal, parameters) 
    
  
    case class MeanvalueEvaluation( das : List[CollectedAction]  
                                  ) extends C1Mapping {

        
      def apply    (x: RealVector)(implicit parameters: Parameters): RealVector = phi(x)
      def remainder(x: RealVector)(implicit parameters: Parameters): RealVector = x
      
      /** Computes the image */
      def phi(x: RealVector)(implicit parameters: Parameters): RealVector = {
        implicit val useIntervalArithmetic: Real[CValue] = intervalBase.cValueIsReal
        implicit val intervalField = intervalBase.FieldImpl(das, Set.empty[CollectedAction], evalExpr)
        val xLift = DynSetEnclosure(x, enc, das.toSet, evalExpr) 
        val imageOfx = xLift map (das.map(d => (d.selfCId, d.lhs.field) -> evalExpr(d.rhs, d.env, xLift)).toMap)
        imageOfx.dynSet
      }

      /** Computes the Jacobian of phi */
      def jacPhi(x: RealVector)(implicit parameters: Parameters): RealMatrix = {
        implicit val useFDifArithmetic: Real[CValue] = fDifBase.cValueIsReal
        val mapVariables = enc.indexToName.values.map{ case (id,n) => QName(id,n) }.toList // used for lifting
        implicit val linearTransformationField = fDifBase.FieldImpl(das.map(_ mapRhs (FAD.lift[CId,CValue](_, mapVariables))), Set.empty[CollectedAction], evalExpr)
        val xLift = DynSetEnclosure(x, enc, das.toSet, evalExpr) 
        val imageOfx = xLift map (das.map(d => (d.selfCId, d.lhs.field) -> evalExpr(d.rhs, d.env, xLift)).toMap)   
        breeze.linalg.Matrix.tabulate[CValue](enc.dim, enc.dim) {
          case (r, c) =>
            (imageOfx dynSet c) match {
              case VLit(GIntervalFDif(FAD.FDif(_, ds))) =>
                val (id, n) = imageOfx indexToName r
                VLit(GConstantRealEnclosure(ds(QName(id, n))))
            }
        }
      }
          
    }

    val myEvaluator = MeanvalueEvaluation(das.toList)
    
    enc.mapping(myEvaluator, evalExpr)
  
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
  def initializeEnclosure(st: CStore) = {
    implicit val parameters = Parameters(st)
    DynSetEnclosure(st)
  }
  def solver = LohnerEnclosureSolver 
  
  case class FieldImpl( odes: List[CollectedAction]
                      , eqsInlined: Set[CollectedAction]
                      , evalExpr: (Expr, Env, EStore) => CValue
                      )( implicit parameters: Parameters
                      ) extends interpreters.Common.Field[DynSetEnclosure,CId] {
    override def apply(dynSetEnclosure: DynSetEnclosure): DynSetEnclosure = {
      val s1 = dynSetEnclosure.dynSet.copy
      odes.foreach{ ode => 
        s1.update(dynSetEnclosure.nameToIndex(ode.lhs.id, ode.lhs.field), evalExpr(ode.rhs, ode.env, dynSetEnclosure)) 
      }
      DynSetEnclosure(s1, dynSetEnclosure, eqsInlined, evalExpr)
    }
    override def variables(s: DynSetEnclosure): List[(CId, Name)] = odes.map(ode => (ode.lhs.id, ode.lhs.field))
    override def map(mE: Expr => Expr) = {
      def mCA(ca: CollectedAction): CollectedAction =
        ca.copy(a = (ca.a: @unchecked) match {
          case Discretely(Assign(lhs: Expr, rhs: Expr)) =>
            Discretely(Assign(mE(lhs), mE(rhs)))
          case Continuously(EquationT(lhs: Expr, rhs: Expr)) =>
            Continuously(EquationT(mE(lhs), mE(rhs)))
          case Continuously(EquationI(lhs: Expr, rhs: Expr)) =>
            Continuously(EquationI(mE(lhs), mE(rhs)))
        })
      FieldImpl(odes map mCA, eqsInlined map mCA, evalExpr)
    }
  }
  
}

