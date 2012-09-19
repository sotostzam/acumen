/* TODO: It would be nice if there is any easy, concise way to make sure that
   we can associate a source program location in each constructor. */

import scala.math._
import scala.collection.mutable.HashMap

package acumen {
 
  /* ==== abstract syntax of Acumen programs ==== */

  /* Names are internalized (hash-consed).
     We cannot rely on scala case classes for that and have to implement
     hashcode, equality, pretty-printing, pattern matching and construction */

  /* Examples:  x, y, etc */
  class Name(val x:String, val primes:Int) {
    override def hashCode = System.identityHashCode(this)
    override def equals(that:Any) = this eq(that.asInstanceOf[AnyRef])
    override def toString = "Name("+x+","+primes+")"
  }
  object Name {
    private case class Key(x:String, p:Int)
    private val cache = HashMap.empty[Key, Name]
    def apply(x:String, p:Int) = {
      val key = Key(x,p)
      cache.getOrElseUpdate(key, new Name(x,p))
    }
    def unapply(x:Name) : Option[(String,Int)] =
      Some(Tuple2(x.x, x.primes))
  }
  
  /* Examples:  Ball, World, etc */
  case class ClassName(x:String)
  /* Example:  class Main () {...}  class Ball (x) {...} */
  case class Prog(defs:List[ClassDef])
  /* Example:  Ball (x,x,x'' ) { ... } */
  case class ClassDef(name:ClassName, fields:List[Name], 
                      priv:List[Init], body:List[Action])
  
  /* Example: x = rhs (inside a private ... end section) */
  case class Init(x:Name, rhs:InitRhs)

  sealed abstract class InitRhs
  /* Example: create Ball(x) */
  case class NewRhs(c:ClassName, fields:List[Expr]) extends InitRhs
  /* Example: 1+2 */
  case class ExprRhs(e:Expr)                        extends InitRhs
  
  sealed abstract class Action
  /* Example: if 1<2 x = 1 else x = 2 end */
  case class IfThenElse(cond:Expr, t:List[Action], e:List[Action]) extends Action 
  /* Example: switch y case 1 x = 2 case 2 x = 3 end */
  case class Switch(subject:Expr, clauses:List[Clause])            extends Action
  /* Example: for i=1:10 x = x+i end */
  case class ForEach(it:Name, col:Expr, body:List[Action])         extends Action
  /* Does not explicitely appear in the syntax */
  case class Continuously(a:ContinuousAction)                      extends Action
  /* Does not explicitely appear in the syntax */
  case class Discretely(a:DiscreteAction)                          extends Action
  
  /* Example: case 1 x = 3; y = 4 */
  case class Clause(lhs:GroundValue, rhs:List[Action])
 
  sealed abstract class ContinuousAction
  /* TODO: use the phase distinction/refinement types trick
           to make sure we get rid of any Equation after the
           desugaring phase */
  /* Example: x'' [=] -g (is desugared into =[t]s and =[i]s) */
  case class Equation(lhs:Expr, rhs:Expr)  extends ContinuousAction
  /* Example: x'' =[i] -g (performs x'' = x'' + -g * dt) */
  case class EquationI(lhs:Expr, rhs:Expr) extends ContinuousAction
  /* Example:  x'' =[t] -g (simple assignment in the continuous phase) */
  case class EquationT(lhs:Expr, rhs:Expr) extends ContinuousAction
  
  sealed abstract class DiscreteAction
  /* Example: x = 3 */
  case class Assign(lhs:Expr, rhs:Expr)             extends DiscreteAction
  /* Example: x = create Ball(1) */
  case class Create(x: Option[Expr], // Some(x) means "x = create ..." 
                    name: ClassName, 
                    args: List[Expr])               extends DiscreteAction 
  /* Example: terminate x */
  case class Elim(e:Expr)                           extends DiscreteAction
  /* Example: move x o */
  case class Move(obj: Expr, newParent: Expr)       extends DiscreteAction
  
  sealed abstract class Expr
  /* Example: 42 (or "a", or 4.2 ...) */
  case class Lit(gv:GroundValue)                      extends Expr
  /* Example: x'' */
  case class Var(name:Name)                           extends Expr
  /* Example: 2+3 */
  case class Op(f:Name, es:List[Expr])                extends Expr
  /* Example: self.x */
  case class Dot(obj:Expr, field:Name)                extends Expr
  /* Example: [1,3,4] */
  case class ExprVector(l:List[Expr])                 extends Expr
  /* Example: sum i*i for i=1:10 if i % 2 == 0 */
  case class Sum(e:Expr, i:Name, col:Expr, cond:Expr) extends Expr
  /* Example: type(Ball) */
  case class TypeOf(cn:ClassName)                     extends Expr
  
  /* ground values (common to expressions and values) */
  
  sealed abstract class GroundValue
  /* Example: 42 */
  case class GInt(i:Int)       extends GroundValue
  /* Example: 4.2e1 */
  case class GDouble(d:Double) extends GroundValue
  /* Example: true */
  case class GBool(b:Boolean)  extends GroundValue
  /* Example: "foo" */
  case class GStr(s:String)    extends GroundValue

  /* ==== values ==== */

  sealed abstract class StepType
  /* (only) example: @Discrete */
  case class Discrete()   extends StepType
  /* (only) example: @Continuous */
  case class Continuous() extends StepType
 
  /* A value is parameterized over the representation of object ids.
     For the reference interpreter, object ids are instances of CId (cf. below).
     For the parallel interpreter, object ids are simply references to scala objects. */
  sealed abstract class Value[+Id]
  /* Example: 42 (or "a", or 4.2 ...) */
  case class VLit[Id](gv:GroundValue)       extends Value[Id]
  /* Example: 1::3::4::nil */
  case class VList[Id](l:List[Value[Id]])   extends Value[Id]
  /* Example: [1,3,4] */
  case class VVector[Id](l:List[Value[Id]]) extends Value[Id]
  /* Example: #0.1.1.2 */
  case class VObjId[Id](a:Option[Id])       extends Value[Id]
  /* Example: Ball */
  case class VClassName[Id](cn:ClassName)   extends Value[Id]
  /* Example: @Continuous */
  case class VStepType[Id](s:StepType)      extends Value[Id]

  /* ==== Canonical (reference) representation of object ids. ==== */

  // 'C' is for 'Canonical'.
 
  /* The id #k1.k2. ... .kn is represented as List(kn,...,k2,k1) for efficiency.
     Moreover, the truly root id is # but it is pretty-printed and parsed as
     #0 to not confuse the user. Similarly, the id #a.b.c.d is pretty-printed
     and parsed as #0.a.b.c.d. Thus, the id printed and parsed as 
     #0.k1.k2. ... .kn is represented as List(kn,...,k2,k1) */
  class CId(val id: List[Int]) extends Ordered[CId] {
    override def hashCode = id.hashCode
    override def equals(that:Any) = that match {
      case cid:CId => id.equals(cid.id)
      case _       => false
    }
    override def toString = (0::id.reverse).mkString(".") 
    def ::(i:Int) : CId = new CId(i::this.id)
    def compare(that: CId) = lex(this.id.reverse, that.id.reverse)
    def lex(xs:List[Int], ys:List[Int]) : Int = {
      if (xs == ys) 0
      else (xs, ys) match {
        case (Nil,_) => -1
        case (_,Nil) => 1
        case (a::as, b::bs) =>
          (Ordering[Int] compare (a,b)) match {
            case 0 => lex(as, bs)
            case n => n
          }
      }
    }
  }

  object CId {
    def nil = new CId(Nil)
    def apply(args:Int*) = new CId(args.toList)
    def unapplySeq(subject:CId) : Option[Seq[Int]] = Some(subject.id)
  }

}

// type aliases have to be declared in a package *object* in scala
package object acumen {
  /* canonical (reference) representation of store/values 
     'C' is for canonical */
  type CValue  = Value[CId]
  type CObject = Map[Name, CValue]
  type CStore  = Map[CId, CObject]
}
