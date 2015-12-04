package acumen
package interpreters
package enclosure2015

import enclosure2015.Common._
import interpreters.Common._
import util._
import util.Canonical._

abstract class DynSetEnclosure extends Enclosure {
  
  val st: CStore
  val dynSet : IntervalDynSet
  def nameToIndex: Map[(CId,Name), Int]
  def indexToName: Map[Int, (CId,Name)]
  
  /** LohnerBase.{ODEEnv,Enclosure}.getObjectField will use this to get values for 
    * non-ODE variables from the (up-to-date) cStore instead of the (out-of-date) dynSet. */
  def nonOdeIndices: Set[Int]
  
  val cachedOuterEnclosure: Option[RealVector]
  
  // FIXME is there a nicer way to do this?
  assert(Set(dynSet.dim, nameToIndex.size, indexToName.size).size == 1)
 
  val dim = nameToIndex.size

  // FIXME should it not take into account the actual variables in the dynset
  lazy val outerEnclosure: RealVector = cachedOuterEnclosure.getOrElse(dynSet)
  
  def init( st: CStore
          , dynSet : IntervalDynSet
          , nameToIndex: Map[(CId,Name), Int]
          , indexToName: Map[Int, (CId,Name)]
          , nonOdeIndices: Set[Int]
          , cachedOuterEnclosure: Option[RealVector] = None
          ): DynSetEnclosure
   
  /** Move the enclosure by the mapping m, returning range and image enclosures. */
  def move( eqsInlined : Set[CollectedAction]
          , flow       : C1Flow
          , evalExpr   : (Expr, Env, EStore) => CValue ) = {
      
    // Update variables in this.cStore defined by continuous assignments w.r.t. odeValues
    def updateCStore(odeValues: RealVector): CStore = {
      def updateODEVariables(unupdated: CStore, odeValues: RealVector): CStore =
        (0 until this.dim).foldLeft(unupdated) {
          case (tmpSt, i) =>
            val (id, n) = this.indexToName(i)
            Canonical.setObjectField(id, n, odeValues(i), tmpSt)
        }
      def updateEquationVariables(unupdated: CStore, odeValues: RealVector): CStore = {
        val odeStore = intervalBase.ODEEnv(cStore, IntervalBox(odeValues), nameToIndex, indexToName, nonOdeIndices, Some(IntervalBox(odeValues)))
        eqsInlined.foldLeft(unupdated){ case (stTmp, ca) =>
          val rd = ca.lhs
          val cv = evalExpr(ca.rhs, ca.env, odeStore)
          Canonical.setObjectField(rd.id, rd.field, cv, stTmp)
        }
      }
      updateEquationVariables(updateODEVariables(this.cStore, odeValues), odeValues)
    }
      
    // Indices in the Lohner set that are not defied by ODEs. These will become invalid after applying the mapping.
    // FIXME These can change over time! Will this cause issues? (see comment for LohnerEnclosure.nonOdeIndices)
    val eqIndices = eqsInlined.map{ ca => val lhs = ca.lhs; this.nameToIndex(lhs.id, lhs.field) }
      
    val (rangeDynSet, endDynSet) = dynSet.move(flow)
        
    val rangeEnclosure = rangeDynSet.outerEnclosure
    val rangeStNext    = updateCStore(rangeEnclosure)
        
    val endEnclosure   = endDynSet.outerEnclosure
    val endStNext      = updateCStore(endEnclosure)
        
    ( init( rangeStNext
          , rangeDynSet                              
          , nameToIndex
          , indexToName
          , eqIndices
          , Some(rangeEnclosure) )
    , init( endStNext
          , endDynSet                              
          , nameToIndex
          , indexToName
          , eqIndices
          , Some(endEnclosure) ) )
      
  }
              
  /* Enclosure interface */
  def cStore = st       
  
  /** Apply m to all CValues in the CStore and Lohner set components */
  def map(m: CValue => CValue): Enclosure =
    init( st.mapValues(_ mapValues m)
        , dynSet.map(m)
        , nameToIndex
        , indexToName
        , nonOdeIndices )
                               
  /** Apply m to all CValues in the CStore and Lohner set components with the 
   *  CId and Name of the value in context */
  def mapName(m: (CId, Name, CValue) => CValue): Enclosure = 
    init( st.map{ case (cid,co) => (cid, co.map{ case (n,v) => (n, m(cid,n,v)) }) }
                , dynSet.map((i: Int, v: CValue) => m(indexToName(i)._1, indexToName(i)._2, v))
                , nameToIndex
                , indexToName
                , nonOdeIndices )

}