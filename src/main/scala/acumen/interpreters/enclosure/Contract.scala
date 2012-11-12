package acumen.interpreters.enclosure

import BinaryRelationName._
import Interval._

trait Contract {

  /** Contracts the environment box for the variables in the relation. */
  def contract(rel: Relation)(env: Box)(implicit rnd: Rounding): Box = {
    //    println("\ncontract: " + rel)
    rel match {
      case BinaryRelation(relname, l, r) => relname match {
        case Eq => contractEq(l, r)(env)
        case Le | Leq => contractLeq(l, r)(env)
      }
    }
  }

  def contractEq(left: Expression, right: Expression)(env: Box)(implicit rnd: Rounding): Box = {
    //    println("\ncontractEq: box before = " + env)
    //    println("contractEq: left       = " + left + " range " + left(env))
    //    println("contractEq: right      = " + right + " range " + right(env))
    val ranl = left(env)
    val ranr = right(env)
    //    println("\ncontractEq: " + ranl + " == " + ranr)
    if (ranl almostEqualTo ranr) {
      //      println("contractEq: " + ranl + " almostEqualTo " + ranr)
      env
    } else {
      val ran = ranl \/ ranr
      //      println("contractEq: backpropagate " + ran)
      val envl = backPropagate(env, ran, left)
      val envr = backPropagate(env, ran, right)
      val contracted = envl intersect envr
      //      println("contractEq: box after  = " + contracted)
      val res =
        if (contracted almostEqualTo env) env
        else contractEq(left, right)(contracted)
      require(env contains res)
      res
    }
  }

  def contractLeq(left: Expression, right: Expression)(env: Box)(implicit rnd: Rounding): Box = {
    //    println("\ncontractLeq: box before = " + env)
    //    println("contractLeq: left       = " + left + " range " + left(env))
    //    println("contractLeq: right      = " + right + " range " + right(env))
    val leftRan = left(env)
    val rightRan = right(env)
    if (leftRan lessThanOrEqualTo rightRan) env
    else {
      require(!(leftRan greaterThan rightRan), left.toString + " <= " + right.toString + " cannot hold over " + env + "!")
      //      println()
      //      println("contractLeq: " + leftRan + " <= " + rightRan)
      val ranl = min(leftRan, rightRan.high)
      //      println("contractLeq: left backpropagate  " + ranl)
      val ranr = max(leftRan.low, rightRan)
      //      println("contractLeq: " + ranl + " <= " + ranr)
      //      println("contractLeq: right backpropagate " + ranr)
      val envl = backPropagate(env, ranl, left)
      val envr = backPropagate(env, ranr, right)
      val contracted = envl intersect envr
      //      println("contractLeq: box after  = " + contracted)
      val res =
        if (contracted almostEqualTo env) env
        else contractLeq(left, right)(contracted)
      require(env contains res)
      res
    }
  }

  /** Contracts the environment box for the variables in the expression. */
  def contract(env: Box, ran: Interval, e: Expression)(implicit rnd: Rounding): Box = {
    val contracted = backPropagate(env, ran, e)
    if (env == contracted) env
    else contract(contracted, ran \/ e(contracted), e)
  }

  object Arg extends Enumeration { type Arg = Value; val Left, Right = Value }; import Arg._

  def backPropagate(env: Box, ran: Interval, expr: Expression)(implicit rnd: Rounding): Box = expr match {
    case Constant(_) => env
    case Variable(name) => env + (name -> env(name) \/ ran)
    case Abs(e) =>
      val enve = e(env)
      (enve disjointFrom ran, enve disjointFrom -ran) match {
        case (false, true) => backPropagate(env, enve \/ ran, e)
        case (true, false) => backPropagate(env, enve \/ (-ran), e)
        case _ => backPropagate(env, enve \/ (ran /\ (-ran)), e)
      }
    case Sqrt(e) => backPropagate(env, e(env) \/ ran.square, e)
    case Negate(e) => backPropagate(env, e(env) \/ (-ran), e)
    case Plus(l, r) =>
      val left = backPropagate(env, l(env) \/ inverseRelation(env, ran, Left, expr), l)
      val right = backPropagate(env, r(env) \/ inverseRelation(env, ran, Right, expr), r)
      left intersect right
    case Multiply(l, r) if l == r =>
      val ransqrt = ran.sqrt
      val envl = l(env)
      (envl disjointFrom ransqrt, envl disjointFrom -ransqrt) match {
        case (false, true) => backPropagate(env, envl \/ ransqrt, l)
        case (true, false) => backPropagate(env, envl \/ (-ransqrt), l)
        case _ => backPropagate(env, envl \/ (ransqrt /\ (-ransqrt)), l)
      }
    case Multiply(Constant(x), e) =>
      val newran = if (x contains 0) e(env) else e(env) \/ (ran / x)
      backPropagate(env, newran, e)
    case Multiply(e, Constant(x)) =>
      val newran = if (x contains 0) e(env) else e(env) \/ (ran / x)
      backPropagate(env, newran, e)
    case Multiply(l, r) =>
      val left = backPropagate(env, l(env) \/ inverseRelation(env, ran, Left, expr), l)
      val right = backPropagate(env, r(env) \/ inverseRelation(env, ran, Right, expr), r)
      left intersect right
    case Divide(e, Constant(x)) => backPropagate(env, e(env) \/ (ran * x), e)
    case Divide(l, r) =>
      val left = backPropagate(env, l(env) \/ inverseRelation(env, ran, Left, expr), l)
      val right = backPropagate(env, r(env) \/ inverseRelation(env, ran, Right, expr), r)
      left intersect right
  }

  def inverseRelation(env: Box, ran: Interval, arg: Arg, e: Expression)(implicit rnd: Rounding): Interval = e match {
    case Plus(l, r) => arg match {
      case Left => ran - r(env)
      case Right => ran - l(env)
    }
    case Multiply(l, r) => arg match {
      case Left => ran / r(env)
      case Right => ran / l(env)
    }
    case Divide(l, r) => arg match { // ran = l / r
      case Left => ran * r(env) // l = ran * r
      case Right => l(env) / ran // r = l / ran
    }
  }

}

object ContractApp extends App with Contract {
  implicit val rnd = Rounding(10)

  println(contract(Box("x" -> Interval(0, 2)), Interval(2), Multiply(Variable("x"), Variable("x"))))

} 
