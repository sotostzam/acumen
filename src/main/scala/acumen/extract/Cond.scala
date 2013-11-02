package acumen
package extract

import scala.collection.mutable.ListBuffer

sealed abstract class Cond { 
  def toExpr: Expr
  def deps : Set[Name]
  // eval assumes have is not False
  def eval(have: Cond) : Cond
  def toSet : Set[Cond] = Set(this)
  def toSeq : Seq[Cond] = toSet.toSeq
  override def toString = Pretty.pprint[Expr](toExpr)
}

object CondImplicits {
  implicit def condToSet(cond: Cond) : Set[Cond] = cond.toSet
  //implicit def condToSeq(cond: Cond) : Seq[Cond] = cond.toSeq
  implicit def condFromSet(conds: Set[Cond]) : Cond = Cond.fromSet(conds)
  implicit def condFromSeq(conds: Seq[Cond]) : Cond = Cond.fromSeq(conds)
}

object Cond {
  import Util._
  // ConImplicits _not_ imported as its best to to the convesion explicitly
  // in the impl., to prevent performance surprises 

  //
  // case class members, constructing them directly is discoursed, use
  // factor methods/constructors below
  // 
  case object True extends Cond {
    def toExpr = Lit(GBool(true))
    def deps = Set.empty
    def eval(have: Cond) = this
    override def toSet = Set.empty[Cond]
  }
  case object False extends Cond {
    def toExpr = Lit(GBool(false))
    def deps = Set.empty
    def eval(have: Cond) = this
  }
  case class MemberOf(name: Name, values: Set[GroundValue]) extends Cond {
    // true if name is a member of values
    def toExpr = values.
      map{v => Op(Name("==", 0), List(Dot(Var(Name("self", 0)), name), Lit(v)))}.
      reduceLeft{ (a, b) => Op(Name("||", 0), List(a, b)) }
    def deps = Set(name)
    def eval(have: Cond) = have.toSet.collectFirst{case MemberOf(n, v) if n == name => v} match {
      case Some(v) => if ((v diff values).isEmpty) True else False
      case _ => if (have.toSet.contains(not(this))) False else this
    }
  }
  case class Not(cond: Cond) extends Cond {
    def toExpr = Op(Name("not", 0), List(cond.toExpr))
    def deps = cond.deps
    def eval(have: Cond) = cond.eval(have) match {
      case True => False
      case False => True
      case res => not(res)
    }
  }
  case class And(conds: Set[Cond]) extends Cond {
    assert(conds.size > 1)
    def toExpr = conds.tail.foldLeft(conds.head.toExpr) { (a, b) => Op(Name("&&", 0), List(a, b.toExpr)) }
    def deps = conds.flatMap{_.deps}
    def eval(have: Cond) = reduce(conds.map{_.eval(have)})
    override def toSet = conds
  }
  case class Other(expr: Expr, deps: Set[Name]) extends Cond {
    def toExpr = expr
    def eval(have: Cond) = 
      if (have.toSet.contains(this)) True
      else if (have.toSet.contains(not(this))) False
      else this
  }

  //
  // Constructors
  //
  def apply(e: Expr): Cond = e match {
    case Lit(GBool(true)) => True
    case Lit(GBool(false)) => False
    case Op(Name("==", 0), List(x, Lit(value))) => eq(x, value)
    case Op(Name("~=", 0), List(x, Lit(value))) => not(eq(x, value))
    case Op(Name("not", 0), List(x)) => not(Cond(x))
    case Op(Name("&&", 0), List(x, y)) => and(Cond(x), Cond(y))
    case _ => Other(e, extractDeps(e))
  }
  def eq(x: Expr, y: Expr): Cond = (x, y) match {
    case (_, Lit(value)) => eq(x, value)
    case _ => Other(Op(Name("==", 0), List(x, (y))), extractDeps(x) ++ extractDeps(y))
  }
  def eq(x: Expr, y: GroundValue): Cond = (getName(x), y) match {
    case (Some(name), _) => MemberOf(name, Set(y))
    case _ => Other(Op(Name("==", 0), List(x, Lit(y))), extractDeps(x))
  }
  def eq(n: Name, y: GroundValue): Cond = MemberOf(n, Set(y))
  def not(cond: Cond): Cond = cond match {
    case Not(cond) => cond
    case _ => Not(cond)
  }
  def not(x: Expr): Cond = not(Cond(x))
  def and(x: Cond, y: Cond) = combine(x, y.toSet)
  def fromSeq(conds: Seq[Cond]) = reduce(conds)
  def fromSet(conds: Set[Cond]) = reduce(conds)


  //
  // Private helper methods
  //
  private def fromSetSimple(conds: Set[Cond]) = conds.size match {
    case 0 => True
    case 1 => conds.head
    case _ => And(conds)
  }
  private def reduce(conds: Iterable[Cond]) = combine(True, conds)
  private def combine(done: Cond, toadd: Iterable[Cond]) : Cond = {
    if (toadd.isEmpty) done
    else (done, toadd.head) match {
      case (True, cond)    => combine(cond, toadd.tail)
      case (cond, True)    => combine(cond, toadd.tail)
      case (False, _)      => False
      case (_, False)      => False
      case (_, And(conds)) => combine(done, conds ++ toadd.tail)
      case (_, cond) if done.toSet.contains(not(cond)) => False
      case (_, MemberOf(name,values)) => done.toSet.collectFirst{case e@MemberOf(n, _) if n == name => e} match {
        case Some(e@MemberOf(_,vls)) => values intersect vls match {
          case res if res.isEmpty => False
          case res => combine(fromSetSimple((done.toSet - e) + MemberOf(name,res)), toadd.tail)
        }
        case None => combine(fromSetSimple(done.toSet + toadd.head) , toadd.tail)}
      case (_, _) => combine(fromSetSimple(done.toSet + toadd.head), toadd.tail)
    }
  }
 
}

