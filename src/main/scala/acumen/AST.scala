/* TODO: It would be nice if there is any easy, concise way to make sure that
   we can associate a source program location in each constructor. */

import scala.math._
import scala.collection.mutable.HashMap
import scala.collection.mutable.{Map => MutMap}
import scala.util.parsing.input.{Positional}
import acumen.interpreters.enclosure.Interval
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

  sealed abstract class ContinuousAction
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
  /* Example: let x=1+2;y=2+3 in x+y end */
  case class ExprLet(bindings:List[(Name,Expr)], e2:Expr) extends Expr
  case class Pattern(ps:List[Expr]) extends Expr
  /* ground values (common to expressions and values) */
  sealed abstract class GroundValue
  /* Example: 42 */
  case class GInt(i: Int) extends GroundValue
  /* Example: 4.2e1 */
  case class GDouble(d: Double) extends GroundValue
  /* Example: [3.1 .. 3.2] */
  case class GInterval(i: Interval) extends GroundValue
  /* Example: true */
  case class GBool(b: Boolean) extends GroundValue
  /* Example: ("fall", (1,2,3))*/
  case class GPattern(p : List[GroundValue]) extends GroundValue
  /* Example: "foo" */
  case class GStr(s: String) extends GroundValue
  /* Representation of a value and its time derivatives */
  abstract class GDif[V] extends GroundValue {
    def dif: TDif[V] 
    def isValidInt: Boolean
    def toInt: Int
    def updated(d: TDif[V]): GDif[V]
  }
  case class GDoubleDif(dif: TDif[Double]) extends GDif[Double] {
    def updated(d: TDif[Double]) = GDoubleDif(d)
    def isValidInt = dif.isValidInt
    def toInt = dif.toInt
  }
  case class GIntDif(dif: TDif[Int]) extends GDif[Int] {
    def updated(d: TDif[Int]) = GIntDif(d)
    def isValidInt = dif.isValidInt
    def toInt = dif.toInt
  }
  case class GIntervalDif(dif: TDif[Interval]) extends GDif[Interval] {
    def updated(d: TDif[Interval]) = GIntervalDif(d)
    def isValidInt = dif.isValidInt
    def toInt = dif.toInt
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
  trait GEnclosure[V] extends GroundValue {
    def apply(t: Interval): V
    def range: V
    def start: V
    def end: V
    def isThin: Boolean
    def show: String
  }
  /* Internal, rigorous representation of a real function */
  abstract class GRealEnclosure extends GEnclosure[Interval]
  /* Internal, rigorous representation of a discrete function */
  abstract class GDiscreteEnclosure[T] extends GEnclosure[Set[T]]
   
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

  /* ==== Canonical (reference) representation of object ids. ==== */

  // 'C' is for 'Canonical'.

  /* The id #k1.k2. ... .kn is represented as List(kn,...,k2,k1) for efficiency.
     Moreover, the truly root id is # but it is pretty-printed and parsed as
     #0 to not confuse the user. Similarly, the id #a.b.c.d is pretty-printed
     and parsed as #0.a.b.c.d. Thus, the id printed and parsed as 
     #0.k1.k2. ... .kn is represented as List(kn,...,k2,k1) */

  trait GId {def cid : CId}

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

}

// type aliases have to be declared in a package *object* in scala
package object acumen {

 /* canonical (reference) representation of (maybe filtered) store/values 
     'C' is for canonical */

  // Notes on the use of CValue/GValue from commit 87bcf66 (Jul 10 2013)
  //
  //   A closely related change is the conversion from using the CStore in
  //   the u.i. and pretty printer to using the new GStore.  A GStore is
  //   similar to a CStore with the following two difference: 1) The
  //   container type is a collection.Iterable instead of immutable.Map and
  //   2) the value type is a generic Value[_] instead of a Value[CId].
  //
  //   With the conversion to using a GStore the results of the imperative
  //   interpreter, with minimal conversion, can be passed around in most
  //   places that expected a CStore.  This change, combined with the default
  //   filtering options, allow the FinalTournamentWithPlayers model to run
  //   again with the imperative interpreters.  By avoiding the unnecessary
  //   conversion to a CStore after each step the imperative interpreter now
  //   runs this model over 4 times faster than the reference interpreter (7s
  //   vs 30s).

  type CValue = Value[CId]
  type CObject = Map[Name, CValue]
  type CStore = Map[CId, CObject]

  // 'G' is for generic
  type GValue = Value[_]
  type GObject = collection.Iterable[(Name, GValue)]
  type GStore = collection.Iterable[(CId, GObject)]
  
  /**
   * Type class with operations that have sensible
   * implementations for Int as well as all the
   * other numeric types.
   */
  trait Integral[V] extends PartialOrdering[V] {
    def add(l: V, r: V): V
    def sub(l: V, r: V): V
    def mul(l: V, r: V): V
    def neg(x: V): V
    def fromInt(i: Int): V
    def toInt(x: V): Int
    def toDouble(x: V): Double
    def zero: V
    def one: V
    def isZero(x: V): Boolean = x == zero
    def isValidInt(x: V): Boolean
    def isValidDouble(x: V): Boolean
    def groundValue(v: V): GroundValue
  }

  /**
   * Type class with operations that have sensible
   * implementations for numeric types that represent
   * real numbers. 
   */
  trait Real[V] extends Integral[V] {
    def div(l: V, r: V): V
    def pow(l: V, r: V): V
    def sin(x: V): V
    def cos(x: V): V
    def tan(x: V): V
    def acos(x: V): V
    def asin(x: V): V
    def atan(x: V): V
    def exp(x: V): V
    def log(x: V): V
    def square(x: V): V
    def sqrt(x: V): V
    def fromDouble(i: Double): V
  }
  
  /** Syntactic sugar for Integral operations */
  implicit class IntegralOps[V](val l: V)(implicit ev: Integral[V]) {
    def +(r: V): V = ev.add(l, r)
    def -(r: V): V = ev.sub(l, r)
    def *(r: V): V = ev.mul(l, r)
    def unary_- = ev.neg(l)
    def < (r: V): Boolean = ev.lt(l, r)
    def > (r: V): Boolean = ev.gt(l, r)
    def <= (r: V): Boolean = ev.lteq(l, r)
    def >= (r: V): Boolean = ev.gteq(l, r)
    def zero: V = ev.zero
    def one: V = ev.one
    def isZero: Boolean = ev.isZero(l)
    def isValidInt: Boolean = ev.isValidInt(l)
    def toInt: Int = ev.toInt(l)
    def isValidDouble: Boolean = ev.isValidDouble(l)
    def toDouble: Double = ev.toDouble(l)
  }

  /** Syntactic sugar for Real operations */
  implicit class RealOps[V](val l: V)(implicit ev: Real[V]) {
    def /(r: V): V = ev.div(l, r)
    def ^(r: V): V = ev.pow(l, r)
    def ^(r: Int): V = ev.pow(l, ev.fromInt(r))
    def sin: V = ev.sin(l)
    def cos: V = ev.cos(l)
    def tan: V = ev.tan(l)
    def acos: V = ev.acos(l)
    def asin: V = ev.asin(l)
    def atan: V = ev.atan(l)
    def exp: V = ev.exp(l)
    def log: V = ev.log(l)
    def square: V = ev.square(l)
    def sqrt: V = ev.sqrt(l)
  }
  
  /** Integral instance for Int */
  implicit object IntIsIntegral extends Integral[Int] {
    def add(l: Int, r: Int): Int = l + r
    def sub(l: Int, r: Int): Int = l - r
    def mul(l: Int, r: Int): Int = l * r
    def neg(x: Int): Int = -x
    def fromInt(x: Int): Int = x
    def zero: Int = 0
    def one: Int = 1
    def tryCompare(l: Int, r: Int): Option[Int] = Some(l compareTo r)
    def lteq(l: Int, r: Int): Boolean = l <= r
    def isValidInt(x: Int): Boolean = true
    def toInt(x: Int): Int = x
    def isValidDouble(x: Int): Boolean = true
    def toDouble(x: Int): Double = x.toDouble
    def groundValue(v: Int): GroundValue = GInt(v)
  }

  /** Real instance for Double */
  implicit object DoubleIsReal extends Real[Double] {
    def add(l: Double, r: Double): Double = l + r
    def sub(l: Double, r: Double): Double = l - r
    def mul(l: Double, r: Double): Double = l * r
    def div(l: Double, r: Double): Double = l / r
    def pow(l: Double, r: Double): Double = Math.pow(l, r)
    def neg(x: Double): Double = -x
    def sin(x: Double): Double = Math.sin(x)
    def cos(x: Double): Double = Math.cos(x)
    def tan(x: Double): Double = Math.tan(x)
    def acos(x: Double): Double = Math.acos(x)
    def asin(x: Double): Double = Math.asin(x)
    def atan(x: Double): Double = Math.atan(x)
    def exp(x: Double): Double = Math.exp(x)
    def log(x: Double): Double = Math.log(x)
    def square(x: Double): Double = x * x
    def sqrt(x: Double): Double = Math.sqrt(x)
    def fromInt(x: Int): Double = x
    def fromDouble(x: Double): Double = x
    def zero: Double = 0
    def one: Double = 1
    def tryCompare(l: Double, r: Double): Option[Int] = Some(l compareTo r)
    def lteq(l: Double, r: Double): Boolean = l <= r
    def isValidInt(x: Double): Boolean =
      Math.floor(x) == Math.ceil(x) && Integer.MIN_VALUE <= x && x <= Integer.MAX_VALUE
    def toInt(x: Double): Int = x.toInt
    def isValidDouble(x: Double): Boolean = true
    def toDouble(x: Double): Double = x
    def groundValue(v: Double): GroundValue = GDouble(v)
  }

  /** Real instance for Interval */
  implicit object IntervalIsReal extends Real[Interval] {
    def add(l: Interval, r: Interval): Interval = l + r
    def sub(l: Interval, r: Interval): Interval = l - r
    def mul(l: Interval, r: Interval): Interval = l * r
    def div(l: Interval, r: Interval): Interval = l / r
    def pow(l: Interval, r: Interval): Interval = l pow r
    def neg(x: Interval): Interval = -x
    def sin(x: Interval): Interval = x.sin
    def cos(x: Interval): Interval = x.cos
    def tan(x: Interval): Interval = x.tan
    def acos(x: Interval): Interval = x.acos
    def asin(x: Interval): Interval = x.asin
    def atan(x: Interval): Interval = x.atan
    def exp(x: Interval): Interval = x.exp
    def log(x: Interval): Interval = x.log
    def square(x: Interval): Interval = x.square
    def sqrt(x: Interval): Interval = x.sqrt
    def fromInt(x: Int): Interval = Interval(x)
    def fromDouble(x: Double): Interval = Interval(x)
    def zero: Interval = Interval.zero
    def one: Interval = Interval.one
    def isValidInt(x: Interval): Boolean = x.lo.doubleValueFloor == x.hi.doubleValueCeiling
    def toInt(x: Interval): Int = x.lo.doubleValueFloor.toInt 
    def isValidDouble(x: Interval): Boolean = (x.lo == x.hi)
    def toDouble(x: Interval): Double = (x.lo).doubleValueFloor
    def lteq(l: Interval, r: Interval): Boolean = l lessThanOrEqualTo r
    def tryCompare(l: Interval, r: Interval): Option[Int] =
      if (l == r)         Some(0)
      else if (lteq(l,r)) Some(1) 
      else if (lteq(r,l)) Some(-1)
      else                None
    def groundValue(v: Interval) = GInterval(v)
  }
  
  /** Representation of a number and its derivatives. */
  abstract class Dif[V : Integral] {
    /** Returns the i:th element of the Dif when i < length
     *  and otherwise the zero of V. */
    def apply(i: Int): V
    /** Returns an integer > 0. */
    def length: Int
    /** Apply m to every element. */
    def map[W: Integral](m: V => W): Dif[W] 
  }
  
  abstract class DifAsIntegral[V: Integral, D <: Dif[V]] {
    /** Factory method */
    def dif(v: Seq[V], length: Int): D
    /* Constants */
    val evVIsIntegral = implicitly[Integral[V]]
    val zeroOfV = evVIsIntegral.zero
    val oneOfV = evVIsIntegral.one
    /* Integral instance */
    protected def combinedLength(l: D, r: D) = Math.min(l.length, r.length)
    def toInt(x: D): Int = x(0).toInt
    def toDouble(x: D): Double = x(0).toDouble
    def isValidInt(x: D): Boolean = evVIsIntegral.isValidInt(x(0)) && isConstant(x)
    def isValidDouble(x: D): Boolean = evVIsIntegral.isValidDouble(x(0)) && isConstant(x)
    def isConstant(x: D): Boolean
    def tryCompare(l: D, r: D): Option[Int] = evVIsIntegral.tryCompare(l(0), r(0))
    def lteq(l: D, r: D): Boolean = evVIsIntegral.lteq(l(0), r(0))
  }


}
