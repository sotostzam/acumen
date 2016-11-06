/* TODO: It would be nice if there is any easy, concise way to make sure that
   we can associate a source program location in each constructor. */

import scala.math._
import scala.collection.mutable.HashMap
import scala.collection.mutable.{Map => MutMap}
import scala.util.parsing.input.{Position,Positional}
import acumen.interpreters.enclosure.{Interval, SplitInterval, SplitterDistribution}
import acumen.TAD._
import acumen.FAD._

package acumen {

  /* ==== abstract syntax of Acumen programs ==== */

  /* Names are internalized (hash-consed).
     We cannot rely on scala case classes for that and have to implement
     hashcode, equality, pretty-printing, pattern matching and construction */

  /* Examples:  x, y, etc */
  class Name(val x: String, val primes: Int) {
    override def hashCode = System.identityHashCode(this)
    override def equals(that: Any) = this eq (that.asInstanceOf[AnyRef])
    override def toString = "Name(" + x + "," + primes + ")"
    def <(that: Name) = Name.nameOrdering.compare(this, that) < 0
  }
  object Name {
    private case class Key(x: String, p: Int)
    private val cache = HashMap.empty[Key, Name]
    def apply(x: String, p: Int) = {
      val key = Key(x, p)
      cache.getOrElseUpdate(key, new Name(x, p))
    }
    def unapply(x: Name): Option[(String, Int)] =
      Some(Tuple2(x.x, x.primes))
    implicit def nameOrdering: Ordering[Name] =
      Ordering.fromLessThan {
      case (Name(x1, ps1), Name(x2, ps2)) =>
      if (x1 == x2) ps1 < ps2 else x1 < x2
      }
  }

  /* Examples:  Ball, World, etc */
  case class ClassName(x: String)
  /* Example:  class Main () {...}  class Ball (x) {...} */
  case class Prog(defs: List[ClassDef])
  /* Example:  Ball (x,x,x'' ) { ... } */
  case class ClassDef(name: ClassName, fields: List[Name],
    priv: List[Init], body: List[Action]) 
    extends Positional
    {var _types : MutMap[Name, TypeLike] = null;}

  /* Example: #semantics "reference2014" */
  case class SemanticsSpec(s: String) extends Positional

  /* Example: #include "ball.acm" */
  case class Include(fn: String) extends Positional

  /* Example: x = rhs (inside a private ... end section) */
  case class Init(x: Name, rhs: InitRhs)

  sealed abstract class InitRhs
  /* Example: create Ball(x) */
  case class NewRhs(c: Expr, fields: List[Expr]) extends InitRhs
  /* Example: create Ball.speed() */
  case class ParaRhs(ex: Expr, nm: Name, fields: List[Expr]) extends InitRhs
  /* Example: 1+2 */
  case class ExprRhs(e: Expr) extends InitRhs

  sealed abstract class Action
  /* Example: if 1<2 x = 1 else x = 2 end */
  case class IfThenElse(cond: Expr, t: List[Action], e: List[Action]) extends Action
  /* Example: switch y case 1 x = 2 case 2 x = 3 end */
  case class Switch(subject: Expr, clauses: List[Clause]) extends Action
  /* Example: for i=1:10 x = x+i end */
  case class ForEach(it: Name, col: Expr, body: List[Action]) extends Action
  /* Does not explicitely appear in the syntax */
  case class Continuously(a: ContinuousAction) extends Action
  /* Does not explicitely appear in the syntax */
  case class Discretely(a: DiscreteAction) extends Action
  /* Example: claim x == 0 && x' <= 0 */
  case class Claim(predicate: Expr) extends Action
  /* Example: hypothesis "x is non-negative" x > 0 */
  case class Hypothesis(statement: Option[String], predicate: Expr) extends Action

  /* Example: case 1 x = 3; y = 4 */
  case class Clause(lhs: GroundValue, assertion: Expr, rhs: List[Action])

  sealed abstract class ContinuousAction extends Positional
  /* TODO: use the phase distinction/refinement types trick
           to make sure we get rid of any Equation after the
           desugaring phase */
  /* Example: x'' = -g (is desugared into =[t]s and =[i]s) */
  case class Equation(lhs: Expr, rhs: Expr) extends ContinuousAction
  /* Example: x'' =[i] -g (performs x'' = x'' + -g * dt) */
  case class EquationI(lhs: Expr, rhs: Expr) extends ContinuousAction
  /* Example:  x'' =[t] -g (simple assignment in the continuous phase) */
  case class EquationT(lhs: Expr, rhs: Expr) extends ContinuousAction
  
  case class Assignment(p:Pattern, rhs:Expr) extends ContinuousAction

  sealed abstract class DiscreteAction extends Positional
  /* Example: x := 3 */
  case class Assign(lhs: Expr, rhs: Expr) extends DiscreteAction
  /* Example: x := create Ball(1) */
  case class Create(x: Option[Expr], // Some(x) means "x = create ..." 
    name: Expr,
    args: List[Expr]) extends DiscreteAction
  /* Example: terminate x */
  case class Elim(e: Expr) extends DiscreteAction
  /* Example: move x o */
  case class Move(obj: Expr, newParent: Expr) extends DiscreteAction
  
  
  
  abstract class TypeEnv {
    def update(name: Name, typ: TypeLike) : Unit
    def get(name: Name) : Option[TypeLike]
  }

  // The position associated with the Expr should point to a
  // significant element of the expression, not necessary the
  // beginning of the expr. See notes for specific types.
  sealed abstract class Expr extends Positional {
    var _lvalue : Option[(TypeEnv, Name)] = None
    var _type : TypeLike = null
  }
  /* Example: 42 (or "a", or 4.2 ...) */
  case class Lit(gv: GroundValue) extends Expr 
  /* Example: x'' */
  case class Var(name: Name) extends Expr
  /* Example x(10) or sin(x) or obj.x(10) */
  case class Call(f: Expr, es: List[Expr]) extends Expr
  /* Note: There is no syntax for Input (it is not parsable)
   *       The sourceId Expr must evaluate to an Int */
  case class Input(sourceId: Expr, inputId: String) extends Expr
  /* Example Main */
  case class Op(f: Name, es: List[Expr]) extends Expr
  /* Example x(10) */
  case class Index(e: Expr, idx: List[Expr]) extends Expr{
    def lhs:Dot = e match{
      case d:Dot => d
    }
  }
  
  /* Reference to field f in object obj. */
  sealed abstract class Ref extends Expr {
    def obj: Expr
    /** The position should point to the field as there is no other way
     to get that position. */
    def field: Name
  } 
  /* Example: self.x */
  case class Dot(obj: Expr, field: Name) extends Ref
  /* Example: a?x */
  case class Quest(obj: Expr, field: Name) extends Ref
  /* Example: self@(0.1:Clazz).x 
   * id with field is a globally unique name (obj has been resolved to id). */
  case class ResolvedDot(id: CId, obj: Expr, field: Name) extends Ref
  /* Example: [1,3,4] */
  case class ExprVector(l: List[Expr]) extends Expr
  /* Example: sum i*i for i=1:10 if i % 2 == 0 */
  case class Sum(e: Expr, i: Name, col: Expr, cond: Expr) extends Expr
  /* Example: type(Ball) */
  case class TypeOf(cn: ClassName) extends Expr
  /* Example: [a:b] deprecated, now [a..b] and m+/-r*/
  case class ExprInterval(lo: Expr, hi: Expr) extends Expr
  case class ExprIntervalM(mid: Expr, pm: Expr) extends Expr


  sealed abstract class ExprSplitter extends Expr

  /**
    * Splitter using a number of equaly sized splits
    * Example: splitby 5
    * @param i bounds of the interval to split
    * @param n number of splits to perform
    */
  case class ExprSplitterN(i: Expr, n: Expr) extends ExprSplitter

  /**
    * splitter using a list of relative weights for the splits size
    * Example: splitby (1, 2, 1, 2, 1)
    * @param i bounds of the interval to split
    * @param ws list of the relativ weights (negative to ignore the corresponding split)
    */
  case class ExprSplitterWeights(i: Expr, ws: List[Expr]) extends ExprSplitter

  /**
    * Splitter using a list of inner points to split the total interval
    * @param ps list of the innerpoints plus the bounds (sorted)
    * @param keeps list of the flags corresponding to the subintervals described by ps. false to ignore a subinterval, true to keep it.
    */
  case class ExprSplitterPoints(ps: List[Expr], keeps: List[Expr]) extends ExprSplitter

  /**
    * Examples: [0 ..1] splitby 5 ; [0 .. 1] splitby (1, 2, 1) ; [0 .. 1] ++ [2 .. 3]
    * @param i ExprInterval correspondong to the total interval
    * @param s ExprSplitter depending on the splitting mechanism used
    */
  case class ExprSplitInterval(i: Expr, s: Expr) extends Expr

  /**
    * Splitters based on distribution. Can generate points or intervals with respect of a distribution
    * Example: noramld(0, 1) central 0.9 splitby 10
    *          uniformd(-5, 5) splitby 5
    *          betad(-10, 10, 0.2, 0.2) splitby 100
    */
  abstract sealed class ExprSplitterDistribution extends Expr

  /**
    * Splitter using a normal distribution
    * Example: noramld(0, 1) central 0.9 splitby 10
    * @param mu mean of the distribution
    * @param sigmaSquared standard deviation squared
    * @param central truncation of the whole distribution aroud the mean (in ]0 .. 1[)
    * @param n number of splits
    */
  case class ExprSplitterNormal(mu: Expr, sigmaSquared: Expr, central: Expr, n: Expr) extends ExprSplitterDistribution

  /**
    * Splitter using a uniform distribution
    * Example: uniformd(-5, 5) splitby 5
    * @param lo low bound of the distribution
    * @param hi high bound
    * @param central truncation of the whole distribution around the middle point (in [0, 1])
    * @param n number of splits
    */
  case class ExprSplitterUniform(lo: Expr, hi: Expr, central: Expr, n: Expr) extends ExprSplitterDistribution

  /**
    * Splitter using a beta distribution
    * Example: betad(-10, 10, 0.2, 0.2) splitby 100
    * @param lo low bound of the distribution
    * @param hi high bound
    * @param a a standard parameter
    * @param b b standard parameter
    * @param central truncation of the whole distribution aroud the mean (in ]0 .. 1])
    * @param n number of splits
    */
  case class ExprSplitterBeta(lo: Expr, hi: Expr, a: Expr, b: Expr, central: Expr, n: Expr) extends ExprSplitterDistribution
  /* Example: let x=1+2;y=2+3 in x+y end */
  case class ExprLet(bindings:List[(Name,Expr)], e2:Expr) extends Expr
  case class Pattern(ps:List[Expr]) extends Expr
  /* ground values (common to expressions and values) */
  sealed abstract class GroundValue extends Positional
  /* GroundValue that wraps a numeric type that has an Integral or Real instance. */
  trait GNumber[V] extends GroundValue
  /* Example: 42 */
  case class GInt(i: Int) extends GNumber[Int]
  /* Example: 4.2e1 */
  case class GDouble(d: Double) extends GNumber[Double]
  /* Example: [3.1 .. 3.2] */
  case class GInterval(i: Interval) extends GNumber[Interval]
  /* Example: true */
  case class GBool(b: Boolean) extends GroundValue
  /* Example: ("fall", (1,2,3))*/
  case class GPattern(p : List[GroundValue]) extends GroundValue
  /* Example: "foo" */
  case class GStr(s: String) extends GroundValue
  /* Representation of a value and its time derivatives */
  abstract class GTDif[V] extends GNumber[V] {
    def dif: TDif[V] 
    def isValidInt: Boolean
    def toInt: Int
    def updated(d: TDif[V]): GTDif[V]
  }
  case class GDoubleTDif(dif: TDif[Double]) extends GTDif[Double] {
    def updated(d: TDif[Double]) = GDoubleTDif(d)
    def isValidInt = dif.isValidInt
    def toInt = dif.toInt
  }
  case class GIntTDif(dif: TDif[Int]) extends GTDif[Int] {
    def updated(d: TDif[Int]) = GIntTDif(d)
    def isValidInt = dif.isValidInt
    def toInt = dif.toInt
  }
  case class GIntervalTDif(dif: TDif[Interval]) extends GTDif[Interval] {
    def apply(x: Interval): Interval = dif.head
    def isThin: Boolean = dif.head.isThin
    def range: Interval = dif.head
    def show = this.toString
    def updated(d: TDif[Interval]) = GIntervalTDif(d)
    def isValidInt = dif.isValidInt
    def toInt = dif.toInt
  }
  case class GCValueTDif(dif: TDif[CValue]) extends GTDif[CValue] {
    def apply(x: Interval): CValue = dif.head
    def show = this.toString
    def updated(d: TDif[CValue]) = GCValueTDif(d)
    def isValidInt = dif.head match {
      case VLit(GConstantRealEnclosure(i)) => intervalIsReal isValidInt i
      case VLit(GIntervalFDif(d)) => intervalFDifIsReal isValidInt d  
    }
    def toInt = dif.head match {
      case VLit(GConstantRealEnclosure(i)) => intervalIsReal toInt i
      case VLit(GIntervalFDif(d)) => intervalFDifIsReal toInt d  
    }
  }
  /* Representation of a value and its partial derivatives w.r.t. the state variables */
  abstract class GFDif[V] extends GroundValue {
    def dif: FDif[V] 
    def isValidInt: Boolean
    def toInt: Int
    def updated(d: FDif[V]): GFDif[V]
  }
  case class GDoubleFDif(dif: FDif[Double]) extends GFDif[Double] {
    def updated(d: FDif[Double]) = GDoubleFDif(d)
    def isValidInt = dif.isValidInt
    def toInt = dif.toInt
  }
  case class GIntFDif(dif: FDif[Int]) extends GFDif[Int] {
    def updated(d: FDif[Int]) = GIntFDif(d)
    def isValidInt = dif.isValidInt
    def toInt = dif.toInt
  }
  case class GIntervalFDif(dif: FDif[Interval]) extends GFDif[Interval] {
    def updated(d: FDif[Interval]) = GIntervalFDif(d)
    def isValidInt = dif.isValidInt
    def toInt = dif.toInt
  }
  /* Representation of an uncertain, time varying value */
  trait GEnclosure[V] extends GNumber[V] {
    def apply(t: Interval): V
    def range: V
    def isThin: Boolean
    def show: String
  }
  /* Internal, rigorous representation of a real function */
  abstract class GRealEnclosure extends GEnclosure[Interval]
  /* Internal, rigorous representation of a discrete function */
  abstract class GDiscreteEnclosure[T] extends GEnclosure[Set[T]] {
    def lift(s: Set[T]): GDiscreteEnclosure[T] 
    def intersect(that: GDiscreteEnclosure[T]): Option[GDiscreteEnclosure[T]] = {
      val i = this.range intersect that.range 
      if (i isEmpty) None else Some(lift(i))
    }
  }
   
  case class GConstantRealEnclosure(range: Interval) extends GRealEnclosure {
    override def apply(t: Interval): Interval = range
    override def isThin: Boolean = range.isThin
    override def show: String = range.toString
    def contains(that: GConstantRealEnclosure): Boolean =
      this.range contains that.range
    def /\ (that: GConstantRealEnclosure): GConstantRealEnclosure =
      GConstantRealEnclosure(this.range /\ that.range)
    def intersect(that: GConstantRealEnclosure): Option[GConstantRealEnclosure] =
      for { e  <- range intersect that.range } yield GConstantRealEnclosure(e)
  }
  object GConstantRealEnclosure {
    def apply(d: Double): GConstantRealEnclosure = GConstantRealEnclosure(Interval(d))
    def apply(i: Int): GConstantRealEnclosure = GConstantRealEnclosure(Interval(i))
  }
  abstract class GConstantDiscreteEnclosure[T](val range: Set[T]) extends GDiscreteEnclosure[T] {
    def apply(t: Interval) = range
    def isThin = range.size == 1
    def show = s"{${range mkString ","}}"
    def contains(that: GConstantDiscreteEnclosure[T]): Boolean =
      that.range subsetOf this.range
  }
  case class GStrEnclosure(override val range: Set[String])
    extends GConstantDiscreteEnclosure[String](range) {
    def lift(s: Set[String]) = GStrEnclosure(s)
  }
  object GStrEnclosure {
    def apply(s: String): GStrEnclosure = GStrEnclosure(Set(s))
  }
  case class GIntEnclosure(override val range: Set[Int]) 
    extends GConstantDiscreteEnclosure[Int](range) {
    def lift(s: Set[Int]) = GIntEnclosure(s)
  }
  case class GBoolEnclosure(override val range: Set[Boolean])  
    extends GConstantDiscreteEnclosure[Boolean](range){
    def lift(s: Set[Boolean]) = GBoolEnclosure(s)
  }
  object GBoolEnclosure {
    def apply(s: Boolean): GBoolEnclosure = GBoolEnclosure(Set(s))
  }
  
  // NOTE: Constants.PI (a GDouble(math.Pi)) is meant as a special
  //   value and is tested for reference equality in
  //   interpreters.enclosure.Extract.  This needs to be taken into
  //   account if GDouble's are ever hash consed.

  /* ==== values ==== */

  sealed abstract class ResultType

  /* (only) example: @Initial */
  case object Initial extends ResultType
  /* (only) example: @Discrete */
  case object Discrete extends ResultType
  /* (only) example @FixedPoint */
  case object FixedPoint extends ResultType
  /* (only) example: @Continuous */
  case object Continuous extends ResultType

  /* A value is parameterized over the representation of object ids.
     For the reference interpreter, object ids are instances of CId (cf. below).
     For the parallel interpreter, object ids are simply references to scala objects. */

  sealed abstract class Value[+Id] extends Positional {
    /** If this is a ground value or collection of ground values:  
     *  Some(i), where i is the number of plots needed to display 
     *  this value. Otherwise None. */
    def yieldsPlots: Option[Int] = None
  }

  /* Example: 42 (or "a", or 4.2 ...) */
  case class VLit(gv: GroundValue) extends Value {
    override def yieldsPlots = Some(1)
  }
  
  sealed abstract class VCollection[+Id] extends Value[Id] {
    def l: List[Value[Id]]
    override def yieldsPlots = l.map(_.yieldsPlots).foldLeft(Some(0): Option[Int]) {
      case (Some(r), Some(i)) => Some(r + i)
      case (None, y) => y
      case (y, None) => y
      case _ => None
    }
  }

  /* Example: 1::3::4::nil */
  case class VList[Id](l: List[Value[Id]]) extends VCollection[Id]

  /* Example: [1,3,4] */
  case class VVector[Id](l: List[Value[Id]]) extends VCollection[Id]

  /* Example: #0.1.1.2 */
  case class VObjId[Id <: GId](a: Option[Id]) extends Value[Id]

  /* Example: Ball */
  case class VClassName(cn: ClassName) extends Value

  /* Example: @Continuous */
  case class VResultType(s: ResultType) extends Value

  /* Annotated version of the AST used for partial evaluation (used internally) */

  case class AProg[A](defs: List[AClassDef[A]])

  case class AClassDef[A](name: ClassName, fields: List[AName[A]], priv: List[AInit[A]], body: List[AAction[A]])

  case class AInit[A](x: AVar[A], rhs: InitRhs)

  sealed abstract class AAction[A] {
    def an: A
  }
  case class AIfThenElse[A](cond: AExpr[A], t: List[AAction[A]], e: List[AAction[A]], val an: A, scope1: A, scope2: A) extends AAction[A]

  case class ASwitch[A](subject: AExpr[A], clauses: List[AClause[A]], val an: A) extends AAction[A]

  case class AForEach[A](it: Name, col: AExpr[A], body: List[AAction[A]], val an: A) extends AAction[A]

  case class AClaim[A](ap: AExpr[A], val an: A) extends AAction[A]

  case class AHypothesis[A](s: Option[String], ap: AExpr[A], val an: A) extends AAction[A]

  case class AContinuously[A](act: AnContinuousAction[A], val an: A) extends AAction[A]

  case class ADiscretely[A](act: AnDiscreteAction[A], val an: A) extends AAction[A]

  case class AClause[A](lhs: GroundValue, assertion: AExpr[A], rhs: List[AAction[A]], a: A, scope: A)

  sealed abstract class AnContinuousAction[A] {
    def an: A
  }
  case class AEquation[A](lhs: AExpr[A], rhs: AExpr[A], val an: A) extends AnContinuousAction[A]
  case class AEquationI[A](lhs: AExpr[A], rhs: AExpr[A], val an: A) extends AnContinuousAction[A]

  case class AEquationT[A](lhs: AExpr[A], rhs: AExpr[A], a: A) extends AnContinuousAction[A] {
    override def an = a
  }
  sealed abstract class AnDiscreteAction[A] {
    def an: A
  }
  case class AAssign[A](lhs: AExpr[A], rhs: AExpr[A], val an: A) extends AnDiscreteAction[A]
  case class ACreate[A](x: Option[Expr], // Some(x) means "x = create ..." 
                        name: Expr,
                        args: List[AExpr[A]], val an: A) extends AnDiscreteAction[A]

  case class AElim[A](e: AExpr[A], val an: A) extends AnDiscreteAction[A]

  case class AMove[A](obj: AExpr[A], newParent: AExpr[A], val an: A) extends AnDiscreteAction[A]

  sealed abstract class AExpr[A]  extends Positional{
    def an: A
    def expr: Expr 
  }
  
  trait AExprWithPos[A]  extends AExpr[A] with Positional {
    def setP(p:Position) = {this.pos = p;this}
  }
  case class ALit[A](gv: GroundValue, val an: A) extends AExpr[A] with AExprWithPos[A]{
    def expr = Lit(gv).setPos(this.pos) 
  }
  case class AVar[A](name: Name, val an: A) extends AExpr[A] with AExprWithPos[A] {
    def expr = Var(name).setPos(this.pos) 
  }

  case class AName[A](name: Name, val an: A) {
  }

  case class AOp[A](f: Name, es: List[AExpr[A]], val an: A) extends AExpr[A] with AExprWithPos[A]{
    def expr = Op(f, es.map(x => x.expr)).setPos(this.pos) 
   }
  case class AIndex[A](e: AExpr[A], idx: List[AExpr[A]], val an: A) extends AExpr[A] with AExprWithPos[A] {
    def expr = Index(e.expr, idx map (_.expr)).setPos(this.pos) 
  }
  case class ADot[A](obj: AExpr[A], field: Name, val an: A) extends AExpr[A] with AExprWithPos[A]{
    def expr = Dot(obj.expr, field).setPos(this.pos) 
  }
  case class AQuest[A](obj: AExpr[A], field: Name, val an: A) extends AExpr[A] with AExprWithPos[A] {
    def expr = Quest(obj.expr, field).setPos(this.pos) 
   }
  case class AExprVector[A](l: List[AExpr[A]], val an: A) extends AExpr[A] with AExprWithPos[A]{
    def expr = ExprVector(l.map(x => x.expr)).setPos(this.pos) 
     }
  case class ASum[A](e: AExpr[A], i: AVar[A], col: AExpr[A], cond: AExpr[A], val an: A) extends AExpr[A] with AExprWithPos[A] {
    def expr = Sum(e.expr, i.expr.name, col.expr, cond.expr).setPos(this.pos) 
  }
  case class ATypeOf[A](cn: ClassName, val an: A) extends AExpr[A]with AExprWithPos[A] {
    def expr = TypeOf(cn).setPos(this.pos) 
      }
  case class AExprInterval[A](lo: AExpr[A], hi: AExpr[A], val an: A) extends AExpr[A] with AExprWithPos[A] {
    def expr = ExprInterval(lo.expr, hi.expr).setPos(this.pos) 
     }
  case class AExprIntervalM[A](mid: AExpr[A], pm: AExpr[A], val an: A) extends AExpr[A] with AExprWithPos[A] {
    def expr = ExprInterval(mid.expr, pm.expr).setPos(this.pos) 
     }
  abstract sealed class AExprSplitter[A] extends AExpr[A] with AExprWithPos[A] 
  case class AExprSplitterN[A](i: AExpr[A], n: AExpr[A], val an: A) extends AExprSplitter[A] {
    def expr = ExprSplitterN(i.expr, n.expr).setPos(this.pos)
  }
  case class AExprSplitterWeights[A](i: AExpr[A], ws: List[AExpr[A]], val an: A) extends AExprSplitter[A] {
    def expr = ExprSplitterWeights(i.expr, ws map (_.expr)).setPos(this.pos)
  }
  case class AExprSplitterPoints[A](ps: List[AExpr[A]], keeps: List[AExpr[A]], val an: A) extends AExprSplitter[A] {
    def expr = ExprSplitterPoints(ps map (_.expr), keeps map (_.expr)).setPos(this.pos)
  }
  case class AExprSplitInterval[A](i: AExpr[A], s: AExpr[A], val an: A) extends AExpr[A] with AExprWithPos[A] {
    def expr = ExprSplitInterval(i.expr, s.expr).setPos(this.pos)
  }
  abstract sealed class AExprSplitterDistribution[A] extends AExpr[A] with AExprWithPos[A] 
  case class AExprSplitterNormal[A](mu: AExpr[A], sigmaSquared: AExpr[A], central: AExpr[A], n: AExpr[A], val an: A) extends AExprSplitterDistribution[A] {
    def expr = ExprSplitterNormal(mu.expr, sigmaSquared.expr, central.expr, n.expr).setPos(this.pos)
  }
  case class AExprSplitterUniform[A](lo: AExpr[A], hi: AExpr[A], central: AExpr[A], n: AExpr[A], val an: A) extends AExprSplitterDistribution[A] {
    def expr = ExprSplitterUniform(lo.expr, hi.expr, central.expr, n.expr).setPos(this.pos)
  }
  case class AExprSplitterBeta[A](lo: AExpr[A], hi: AExpr[A], a: AExpr[A], b: AExpr[A], central: AExpr[A], n: AExpr[A], val an: A) extends AExprSplitterDistribution[A] {
    def expr = ExprSplitterBeta(lo.expr, hi.expr, a.expr, b.expr, central.expr, n.expr).setPos(this.pos)
  }
  case class AExprLet[A](bindings: List[(AVar[A], AExpr[A])], e2: AExpr[A], val an: A) extends AExpr[A] with AExprWithPos[A]{
    def expr = ExprLet(bindings.map(x => (x._1.name, x._2.expr)), e2.expr).setPos(this.pos) 
    }
  case class ACall[A](f: AExpr[A], es: List[AExpr[A]], an: A) extends AExpr[A]with AExprWithPos [A]{
    def expr = Call(f.expr, es.map(_.expr)).setPos(this.pos) 
   }

  /* Constraints for binding time analysis. Solving them
   * would result in assigning static/dynamic for each label */
  sealed abstract class Constraint

  /* l1 must happen no later than l2(l1 <= l2) */
  case class NLT(val label: Int, val l2: Int) extends Constraint

  case class Equals(val l1: Int, val l2: Int) extends Constraint

  /* Use for determine equation known or not by the n-1 variables in it*/
  case class NLTS(val label: Int, val l2: List[Int]) extends Constraint

  case class Known(val label: Int) extends Constraint

  case class Unknown(val label: Int) extends Constraint
  


  /* ==== Canonical (reference) representation of object ids. ==== */

  // 'C' is for 'Canonical'.

  /* The id #k1.k2. ... .kn is represented as List(kn,...,k2,k1) for efficiency.
     Moreover, the truly root id is # but it is pretty-printed and parsed as
     #0 to not confuse the user. Similarly, the id #a.b.c.d is pretty-printed
     and parsed as #0.a.b.c.d. Thus, the id printed and parsed as 
     #0.k1.k2. ... .kn is represented as List(kn,...,k2,k1) */

  trait GId {def cid : CId}
  
  case class QName(id: GId, n: Name)

  class CId(val id: List[Int]) extends Ordered[CId] with GId {
    def cid = this
    override def hashCode = id.hashCode
    override def equals(that: Any) = that match {
      case cid: CId => id.equals(cid.id)
      case _ => false
    }
    override def toString = (0 :: id.reverse).mkString(".")
    def ::(i: Int): CId = new CId(i :: this.id)
    def compare(that: CId) = lex(this.id.reverse, that.id.reverse)
    def lex(xs: List[Int], ys: List[Int]): Int = {
      if (xs == ys) 0
      else (xs, ys) match {
        case (Nil, _) => -1
        case (_, Nil) => 1
        case (a :: as, b :: bs) =>
          (Ordering[Int] compare (a, b)) match {
            case 0 => lex(as, bs)
            case n => n
          }
      }
    }
  }

  object CId {
    def nil = new CId(Nil)
    def apply(args: Int*) = new CId(args.toList)
    def unapplySeq(subject: CId): Option[Seq[Int]] = Some(subject.id)
  }

  /* Type system */
  abstract class TypeLike {
    // numericLike and finalized are here becuase pattern matching on
    // abstract base classes and traits is buggy in Scala
    def numericLike = false
    final def isVector = vectorSize != 1
    def vectorSize = -1
    def finalized : Boolean
    def classLike = false
    def classSubType : ClassSubType = null
  }

  sealed abstract class Type extends TypeLike {
    override def finalized = true
  }

  case object DynamicType extends Type     // conflicting types (bottom)
  
  trait NumericLike extends TypeLike {
    override def numericLike = true
  }

  sealed abstract class SomeNumericType extends Type with NumericLike
  case object IntType extends SomeNumericType
  case object NumericType extends SomeNumericType
  // normally Double but can also be an interval or enclosure

  case object BoolType extends Type
  case object StrType extends Type

  sealed abstract class SeqSubType {
    def map(f: TypeLike => TypeLike) = this match {
      case DynamicSize(typ)   => DynamicSize(f(typ))
      case FixedSize(typs)    => FixedSize(typs.map(f))
    }
    def size = this match {
      case DynamicSize(typ)   => None
      case FixedSize(typs)    => typs.size
    }
    def foreach(f: TypeLike => TypeLike) : Unit = map(f)
    def zip(that: SeqSubType, f: (TypeLike, TypeLike) => TypeLike) = (this, that) match {
      case (DynamicSize(x), DynamicSize(y)) => DynamicSize(f(x,y))
      case (FixedSize(x), FixedSize(y))     => FixedSize(x.zip(y).map{case (a,b) => f(a,b)})
      case _                                => null
    }
    def isNumeric = this match {
      case DynamicSize(_:NumericLike) => true
      case FixedSize(typs) => typs.forall{t => t match {case _:NumericLike => true; case _ => false}}
      case _ => false
    }
    def uniformType = this match {
      case DynamicSize(t) => Some(t)
      case FixedSize(h::t) => if (t.forall(_ == h)) Some(h) else None
      case _ => None
    }
    override def toString = this match {
      case DynamicSize(t) => t.toString
      case FixedSize(typs) => uniformType match {
        case Some(null) => "<error>"
        case Some(typ)  => typ.toString + " x " + typs.size
        case None       => typs.toString
      }
    }
  }
  case class DynamicSize(typ: TypeLike) extends SeqSubType
  case class FixedSize(typs: List[TypeLike]) extends SeqSubType

  case class SeqType(subType: SeqSubType) extends Type // FIXME: Lie
  // SeqType represents both vectors and lists, sz is None is the size
  // is not known at compile time
  {
    override def vectorSize = 
      subType match {
        case FixedSize(l) if subType.isNumeric => l.size
        case _                                 => -1
      }
  }

  // case object ObjIdType extends Type -- not used
  case object ResultTypeType extends Type
  //case object ClassNameType extends Type -- overkill for now

  sealed abstract class ClassSubType 
  case object BaseClass extends ClassSubType
  case object DynamicClass extends ClassSubType
  case class NamedClass(cn: ClassName) extends ClassSubType

  case class ClassType(subType: ClassSubType) extends Type {
    override def classLike = true
    override def classSubType = subType
    override def toString = subType match {
      case BaseClass => "BaseClass"
      case DynamicClass => "DynamicClass"
      case NamedClass(cn) => cn.x
    }
  }

  /**
    * Tag gives informations about the Store to which it is attached and is supposed to be a unique identifier for it.
    * It tells which values from a split have been kept (additional information can be added to this class)
    * CId + Name: Identify the interval which has been split
    * 1st Int: Identify the subinterval (or subpoint) kept to build the Store
    * 2nd Int: Number of split done on the corresponding value
    * Interval: Bounds the probability of the corresponding subinterval resulting from the split
    * @param splitInfo split information of the store to wich the tag s attached
    */
  case class Tag(splitInfo: List[((CId, Name, Int, Int), Option[Interval])]) extends Ordered[Tag] {
    require(splitInfo.forall(_._2.isEmpty) || splitInfo.forall(Interval(0, 1) contains _._2.get),
      "Probabilities must all be between 0 and 1 or all be undefined")

    def pretty = {
      if (this == Tag.root)
        ""
      else {
        val body =
          if (splitInfo.isEmpty) ""
          else splitInfo.map { case ((id, Name(x, p), i1, i2), _) => x + "'" * p + ":" + i1 + "/" + i2 }.mkString(", ")
        val proba = if (p.nonEmpty) ", p=[%.2f..%.2f]".format(p.get.loDouble, p.get.hiDouble) else ""
        "(" + body + proba + ")"
      }
    }
    override def toString = {
      val body = if(splitInfo.nonEmpty)
                    splitInfo.map{case ((_, n, i1, i2), _) => "(" + n.x + ", " + n.primes + "), " + i1 + "/" + i2 }.mkString(", ")
                 else "root"
      val proba = if (p.nonEmpty) ", p=[%.2f..%.2f]".format(p.get.loDouble, p.get.hiDouble) else ""
      "Tag(" + body + proba+ ")"
    }

    /**
      * probability of the Store to which the tag is attached
      */
    lazy val p: Option[Interval] = if (splitInfo.isEmpty || splitInfo.exists(_._2.isEmpty)) None else splitInfo.unzip._2. reduce ((l, r) => Some(l.get * r.get))

    def ::(id: (CId, Name, Int, Int), p: Option[Interval] = None) = Tag((id, p)::splitInfo)
    def ::(t: Tag) = Tag(t.splitInfo ::: splitInfo)

    /**
      * Defines a meaningless total order (no mathematical justification)
      * Sorting tags is useful only to make the simulations reproducible despite the fact that Store are stored in maps
      * which are not sorted.
      * It is used to write the result file of a simulation in a reproducible way for the tests for instance
      * @param that element to which this is compared
      * @return 1 if this > that, -1 if this < that, 0 otherwise
      */
    override def compare(that: Tag): Int = {
      val zippedTags = splitInfo.zip(that.splitInfo)
      var i = 0
      while(i < zippedTags.length) {
        zippedTags(i) match {
          case (((cid1, name1, i1, t1), p1), ((cid2, name2, i2, t2), p2)) =>
            if (cid1 > cid2) return 1 else if (cid1 < cid2) return -1
            if (name2 < name1) return 1 else if (name1 < name2) return -1
            if (i1 > i2) return 1 else if (i1 < i2) return -1
            if (t1 > t2) return 1 else if (t1 < t2) return -1
            (p1, p2) match {
              case (Some(_), None) => return 1
              case (None, Some(_)) => return -1
              case (Some(p1), Some(p2)) => if (p1 greaterThan p2) return 1 else if (p1 lessThan p2) return -1
              case _ => //Continue
            }
        }
        i += 1
      }
      0
    }
  }

  object Tag {
    def root = Tag(List.empty)
  }

}
