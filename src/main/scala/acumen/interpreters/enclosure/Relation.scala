package acumen.interpreters.enclosure

import Expression._
import Interval._
import Types._
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure

object BinaryRelationName extends Enumeration {
  type BinaryRelationName = Value
  val Le, Leq, Eq, Neq = Value
}; 
import BinaryRelationName._

object UnaryRelationName extends Enumeration {
  type UnaryRelationName = Value
  val Positive, NonNegative, EqualToZero, NonPositive, Negative = Value
}; 
import UnaryRelationName._

/**
 * Type used to represent relations used to define predicates.
 *
 * Implementation note: we are initially aiming to support the benchmarks from
 * the paper "Enclosing Hybrid Behavior". These benchmark hybrid systems have
 * limited range of predicates appearing as event guards and mode invariants.
 * In particular all these predicates are of the form of an inequality between
 * 0 and a variable. Handling of more complex expressions in inequalities will
 * require constraint solving to deduce the support of predicates used in the
 * solveVt algorithm.
 */
abstract class Relation extends Contract {

  // TODO add comment
  def compose(that: Expression, intoVariable: String): Relation = this match {
    case BinaryRelation(relname, l, r) => BinaryRelation(relname, l.compose(that, intoVariable), r.compose(that, intoVariable))
    case UnaryRelation(relname, e) => UnaryRelation(relname, e.compose(that, intoVariable))
  }

  /**
   * Evaluate the predicate by taking the variables to range over the intervals
   * of the box x.
   *
   * Note that the result will be a set of Boolean values rather than a single
   * one as the predicate is evaluated over intervals, i.e. sets of reals.
   */
  def apply(x: Box): Set[Boolean] = this match {
    case BinaryRelation(relname, l, r) =>
      {
        val left = l(x)
        val right = r(x)
        relname match {
          case Le if l == r => Set(false)
          case Le =>
            if (left.high lessThan right.low) Set(true)
            else if (left.low greaterThanOrEqualTo right.high) Set(false)
            else Set(true, false)
          case Leq =>
            if (left.high lessThanOrEqualTo right.low) Set(true)
            else if (left.low greaterThan right.high) Set(false)
            else Set(true, false)
          //          case Eq if l == r => Set(true)
          case Eq =>
            if (left == right && left.isThin) Set(true)
            else if (left disjointFrom right) Set(false)
            else Set(true, false)
          case Neq =>
            if (left == right && left.isThin) Set(false)
            else if (left disjointFrom right) Set(true)
            else Set(true, false)
        }
      }
    case UnaryRelation(relname, e) => {
      val value = e(x)
      relname match {
        case Positive => {
          if (value greaterThan 0) Set(true)
          else if (value lessThanOrEqualTo 0) Set(false)
          else Set(true, false)
        }
        case NonNegative => {
          if (value greaterThanOrEqualTo 0) Set(true)
          else if (value lessThan 0) Set(false)
          else Set(true, false)
        }
        case EqualToZero => {
          if (value equalTo 0) Set(true)
          else if (value contains 0) Set(true, false)
          else Set(false)
        }
        case NonPositive => {
          if (value lessThanOrEqualTo 0) Set(true)
          else if (value greaterThan 0) Set(false)
          else Set(true, false)
        }
        case Negative => {
          if (value lessThan 0) Set(true)
          else if (value greaterThanOrEqualTo 0) Set(false)
          else Set(true, false)
        }
        case _ => sys.error("Relation.eval: " + this.toString)
      }
    }
  }

  // TODO do something about the code duplication in these instances!
  /**
   * Evaluate the relation by composing with the enclosure and taking the variables
   * to range over the intervals of the box x.
   *
   * Note that the result will be a set of Boolean values rather than a single one
   * as the relation is evaluated over the domains of variables, i.e. sets of reals.
   */
  def apply(x: UnivariateAffineEnclosure): Set[Boolean] = this match {
    case BinaryRelation(relname, l, r) =>
      val left = l(x)
      val right = r(x)
      val (lo, hi) = x.domain.bounds
      relname match {
        case Le =>
          if ((left(lo) lessThan right(lo)) && (left(hi) lessThan right(hi))) Set(true)
          else if ((left(lo) greaterThanOrEqualTo right(lo)) && (left(hi) greaterThanOrEqualTo right(hi))) Set(false)
          else Set(true, false)
        case Leq =>
          if ((left(lo) lessThanOrEqualTo right(lo)) && (left(hi) lessThanOrEqualTo right(hi))) Set(true)
          else if ((left(lo) greaterThan right(lo)) && (left(hi) greaterThan right(hi))) Set(false)
          else Set(true, false)
        case Eq =>
          if (((left(lo) lessThanOrEqualTo right(lo)) && (left(hi) greaterThanOrEqualTo right(hi))) ||
            ((left(lo) greaterThanOrEqualTo right(lo)) && (left(hi) lessThanOrEqualTo right(hi)))) Set(true)
          else if (((left(lo) lessThan right(lo)) && (left(hi) lessThan right(hi))) ||
            ((left(lo) greaterThan right(lo)) && (left(hi) greaterThan right(hi)))) Set(false)
          else Set(true, false)
      }
    //    case BinaryRelation(relname, l, r) => relname match {
    //      case Le => UnaryRelation(Negative, l - r)(x)
    //      case Leq => UnaryRelation(NonPositive, l - r)(x)
    //      case Eq => UnaryRelation(EqualToZero, l - r)(x)
    //    }
    case UnaryRelation(relname, e) => {
      val value = e(x).range
      relname match {
        case Positive => {
          if (value greaterThan 0) Set(true)
          else if (value lessThanOrEqualTo 0) Set(false)
          else Set(true, false)
        }
        case NonNegative => {
          if (value greaterThanOrEqualTo 0) Set(true)
          else if (value lessThan 0) Set(false)
          else Set(true, false)
        }
        case EqualToZero => {
          if (value equalTo 0) Set(true)
          else if (value contains 0) Set(true, false)
          else Set(false)
        }
        case NonPositive => {
          if (value lessThanOrEqualTo 0) Set(true)
          else if (value greaterThan 0) Set(false)
          else Set(true, false)
        }
        case Negative => {
          if (value lessThan 0) Set(true)
          else if (value greaterThanOrEqualTo 0) Set(false)
          else Set(true, false)
        }
        case _ => sys.error("Relation.eval: " + this.toString)
      }
    }
  }

  /** A conservative approximation of the intersection of x with the support of r. */
  def support(x: Box): Box = {
    this match {
      case BinaryRelation(relname, l, r) =>
        //        println("support of: " + this)
        //        println("over:       " + x)
        val res = contract(this)(x)
        require(x contains res)
        //        println("is:         " + res)
        res
      case UnaryRelation(relname, e) =>
        //      println("Relation:      " + this)
        //      println("Box:           " + x)
        var res = x
        relname match {
          case Positive | NonNegative =>
            val ran = e(x)
            if (ran.high greaterThanOrEqualTo 0) res = e.contractBox(x, max(ran, 0))
            else sys.error("empty support: " + ran + " /\\ " + max(ran, 0))
          case EqualToZero => res = e.contractBox(x, 0)
          case NonPositive | Negative =>
            val ran = e(x)
            if (ran.low lessThanOrEqualTo 0) res = e.contractBox(x, min(ran, 0))
            else sys.error("empty support" + ran + " /\\ " + min(ran, 0))
        }
        //      res = supportBySplitting(5)(res) match {
        //        case Some(box) => box
        //        case None => sys.error("empty support for " + this + " in " + x)
        //      }
        //      println("Contracted:    " + res)
        //      println("##############")

        //      val rNN = max(res("r"),0)
        //      val xNN = max(res("x"),0)
        //      val absV = (rNN-xNN*20).abs
        //      val x2 = (rNN-(res("v")*res("v")).abs)/20
        //      val invariant = Box("x" -> (xNN \/ x2), "x'" -> (res("x'") \/ (absV /\ -absV)), "r" -> rNN)
        //      res = res intersect invariant

        res
    }
  }

  /** TODO add description! */
  def supportBySplitting(maxDepth: Int)(box: Box): Option[Box] = {
    require(maxDepth >= 0 && varNames.subsetOf(box.keySet))
    def supportHelper(supportBoxes: Seq[Box], queue: Seq[(Int, Box)]): Seq[Box] = queue match {
      case (depth, b) :: bs =>
        if (depth < maxDepth) this(b).toSeq match {
          case Seq(false) => supportHelper(supportBoxes, bs)
          case Seq(true) => supportHelper(supportBoxes :+ b, bs)
          case _ => supportHelper(supportBoxes, bs ++ b.split.toSeq.map((depth + 1, _)))
        }
        else supportHelper(supportBoxes :+ b, bs)
      case _ => supportBoxes ++ queue.map(_._2)
    }
    supportHelper(Seq(), Seq((0, box))) match {
      case b :: bs => Some(bs.foldLeft(b)(_ hull _))
      case _ => None
    }
  }

  /** Returns the set of variable names which occur in the relation. */
  def varNames: Set[VarName] = this match {
    case UnaryRelation(_, expression) => expression.varNames
  }

  /**
   * Tests if the relation is one of the reflexive inequalities, i.e.
   * of the form 0 <= e or e <= 0 for some expression e.
   */
  def isNonStrict: Boolean = this match {
    case UnaryRelation(n, _) => n match {
      case NonPositive => true
      case NonNegative => true
      case _ => false
    }
  }

}
object Relation {
  def lessThan(left: Expression, right: Expression) = BinaryRelation(Le, left, right)
  def lessThanOrEqualTo(left: Expression, right: Expression) = BinaryRelation(Leq, left, right)
  def equalTo(left: Expression, right: Expression) = BinaryRelation(Eq, left, right)
  def notEqualTo(left: Expression, right: Expression) = BinaryRelation(Neq, left, right)
  def positive(that: Expression) = UnaryRelation(Positive, that)
  def nonNegative(that: Expression) = UnaryRelation(NonNegative, that)
  def equalToZero(that: Expression) = UnaryRelation(EqualToZero, that)
  def nonPositive(that: Expression) = UnaryRelation(NonPositive, that)
  def negative(that: Expression) = UnaryRelation(Negative, that)
}

case class BinaryRelation(name: BinaryRelationName, left: Expression, right: Expression) extends Relation {
  override def toString = name match {
    case Le => left.toString + " < " + right.toString
    case Leq => left.toString + " <= " + right.toString
    case Eq => left.toString + " == " + right.toString
    case Neq => left.toString + " ~= " + right.toString
  }
}

case class UnaryRelation(name: UnaryRelationName, expression: Expression) extends Relation {
  override def toString = name match {
    case Positive => expression.toString + " > 0"
    case NonPositive => expression.toString + " <= 0"
    case EqualToZero => expression.toString + " = 0"
    case NonNegative => expression.toString + " >= 0"
    case Negative => expression.toString + " < 0"
  }
}

object RelationApp extends App {
  implicit val rnd = Parameters.default.rnd
  val dom = Box("x" -> Interval(0, 0.4890632644), "v" -> Interval(-4.014587403, 1.069793702), "r" -> Interval(0, 1.562500000))
  val x: Expression = "x"
  val v: Expression = "v"
  val r: Expression = "r"
  val rel = Relation.nonPositive(r - ((v * v) + 20 * x))
  println(rel)
  println(dom)
  println(rel.support(dom))
}

object SupportApp extends App {
  implicit val rnd = Parameters.default.rnd
  val b = Box("r" -> Interval(0, 1), "v" -> Interval(-1, 1), "x" -> Interval(0.5, 1))
  val r = Variable("r")
  val v = Variable("v")
  val x = Variable("x")
  println(Relation.equalTo(Abs(v), Sqrt(Abs(r - 20 * x))).support(b))
}
