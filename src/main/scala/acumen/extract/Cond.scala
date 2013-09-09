package acumen
package extract

import scala.collection.mutable.ListBuffer

sealed abstract class Cond { 
  def toExpr: Expr
  def deps : Seq[Name]
  // eval assumes have is not False
  def eval(have: Cond) : Cond
  def toSeq : Seq[Cond] = Seq(this)
  override def toString = Pretty.pprint[Expr](toExpr)
}

object CondAsSeq {
  implicit def condToSeq(cond: Cond) : Seq[Cond] = cond.toSeq
  implicit def condFromSeq(conds: Seq[Cond]) : Cond = Cond.fromSeq(conds)
}

object Cond {
  import Util._
  // CondAsSeq _not_ imported as its best to to the convesion explicitly
  // in the impl., to prevent performance surprises 

  //
  // case class members, constructing them directly is discoursed, use
  // factor methods/constructors below
  // 
  case object True extends Cond {
    def toExpr = Lit(GBool(true))
    def deps = Nil
    def eval(have: Cond) = this
    override def toSeq = Nil
  }
  case object False extends Cond {
    def toExpr = Lit(GBool(false))
    def deps = Nil
    def eval(have: Cond) = this
  }
  case class Eq(name: Name, value: GroundValue) extends Cond {
    def toExpr = Op(Name("==", 0), List(Dot(Var(Name("self", 0)), name), Lit(value)))
    def deps = Seq(name)
    def eval(have: Cond) = have.toSeq.collectFirst{case Eq(n, v) if n == name => v} match {
      case Some(v) => if (v == value) True else False
      case None => if (have.toSeq.contains(not(this))) False else this
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
  case class And(conds: Seq[Cond]) extends Cond {
    assert(conds.length > 1)
    assert(conds(0) != True)
    def toExpr = conds.tail.foldLeft(conds.head.toExpr) { (a, b) => Op(Name("&&", 0), List(a, b.toExpr)) }
    def deps = conds.flatMap{_.deps}.distinct
    def eval(have: Cond) = reduce(conds.map{_.eval(have)})
    override def toSeq = conds
  }
  case class Other(expr: Expr, deps: Seq[Name]) extends Cond {
    def toExpr = expr
    def eval(have: Cond) = 
      if (have.toSeq.contains(this)) True
      else if (have.toSeq.contains(not(this))) False
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
    case _ => Other(e, extractDeps(e))
  }
  def eq(x: Expr, y: Expr): Cond = (x, y) match {
    case (_, Lit(value)) => eq(x, value)
    case _ => Other(Op(Name("==", 0), List(x, (y))), extractDeps(x) ++ extractDeps(y))
  }
  def eq(x: Expr, y: GroundValue): Cond = (getName(x), y) match {
    case (Some(name), _) => Eq(name, y)
    case _ => Other(Op(Name("==", 0), List(x, Lit(y))), extractDeps(x))
  }
  def not(cond: Cond): Cond = cond match {
    case Not(cond) => cond
    case _ => Not(cond)
  }
  def not(x: Expr): Cond = not(Cond(x))
  def and(x: Cond, y: Cond) = fromSeq(List(x,y))
  def fromSeq(conds: Seq[Cond]) = reduce(conds)

  //
  // Private helper methods
  //
  private def fromSeqSimple(conds: Seq[Cond]) = {conds.lengthCompare(1) match {
    case x if x < 0  => True
    case x if x == 0 => conds.head
    case x if x > 0  => And(conds)
  }}
  private def reduceSimple(conds: Seq[Cond]) : Cond = {
    val res = ListBuffer.empty[Cond]
    conds.foreach{
      case True => /* do nothing */
      case False => return False
      case And(els) => res ++= els
      case cond => res += cond
    }
    fromSeqSimple(res.toList)
  }
  private def reduce(conds: Seq[Cond]) : Cond = {
    val res = ListBuffer.empty[Cond]
    conds.foreach{ _.eval(fromSeqSimple(res)) match {
      case True => /* do nothing */
      case False => return False
      case And(els) => res ++= els
      case cond => res += cond
    }}
    fromSeqSimple(res.toList)
  }
}

