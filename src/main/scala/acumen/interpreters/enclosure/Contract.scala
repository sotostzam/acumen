package acumen.interpreters.enclosure

import BinaryRelationName._
import Interval._

trait Contract {

  /** Contracts the environment box for the variables in the relation. */
  def contract(rel: Relation)(env: Box): Box = {
    rel match {
      case BinaryRelation(relname, l, r) => relname match {
        case Eq       => contractEq(l, r)(env)
        case Neq      => contractNeq(l, r)(env)
        case Le | Leq => contractLeq(l, r)(env)
      }
    }
  }

  def contractEq(left: Expression, right: Expression)(env: Box): Box = {
    val ranl = left(env)
    val ranr = right(env)
    if (ranl almostEqualTo ranr) {
      env
    }
    else {
      val ran = ranl \/ ranr
      val envl = backPropagate(env, ran, left)
      val envr = backPropagate(env, ran, right)
      val contracted = envl \/ envr
      val res =
        if (contracted almostEqualTo env) env
        else contractEq(left, right)(contracted)
      require(env contains res)
      res
    }
  }

  def contractNeq(left: Expression, right: Expression)(env: Box): Box = {
    val ranl = left(env)
    val ranr = right(env)
    if (!ranl.isThin || !ranr.isThin || !ranl.equalTo(ranr)) env
    else sys.error("contracted to empty box") // only when both lhs and rhs are thin can equality be established
  }

  def contractLeq(left: Expression, right: Expression)(env: Box): Box = {
    val leftRan = left(env)
    val rightRan = right(env)
    if (leftRan lessThanOrEqualTo rightRan) env
    else {
      require(!(leftRan greaterThan rightRan), "contractLeq: " + left.toString + " <= " + right.toString + " cannot hold over " + env + "!")
      val ranl = min(leftRan, rightRan.high)
      val ranr = max(leftRan.low, rightRan)
      val envl = backPropagate(env, ranl, left)
      val envr = backPropagate(env, ranr, right)
      val contracted = envl \/ envr
      val res =
        if (contracted almostEqualTo env) env
        else contractLeq(left, right)(contracted)
      require(env contains res)
      res
    }
  }

  /** Contracts the environment box for the variables in the expression. */
  def contract(env: Box, ran: Interval, e: Expression): Box = {
    val contracted = backPropagate(env, ran, e)
    if (env == contracted) env
    else contract(contracted, ran \/ e(contracted), e)
  }

  object Arg extends Enumeration { type Arg = Value; val Left, Right = Value }; import Arg._

  def backPropagate(env: Box, ran: Interval, expr: Expression): Box = expr match {
    case Constant(_)    => env
    case Variable(name) => env + (name -> env(name) \/ ran)
    case Abs(e) =>
      val enve = e(env)
      (enve disjointFrom ran, enve disjointFrom -ran) match {
        case (false, true) => backPropagate(env, enve \/ ran, e)
        case (true, false) => backPropagate(env, enve \/ (-ran), e)
        case _             => backPropagate(env, enve \/ (ran /\ (-ran)), e)
      }
    case Negate(e) => backPropagate(env, e(env) \/ (-ran), e)
    case Sqrt(e)   => backPropagate(env, e(env) \/ ran.square, e)
    case Exp(e)    => throw new NotImplementedError("Contract.backPropagate.exp")
    case Log(e)    => throw new NotImplementedError("Contract.backPropagate.log")
    case Sin(e)    => throw new NotImplementedError("Contract.backPropagate.sin")
    case Cos(e)    => throw new NotImplementedError("Contract.backPropagate.cos")
    case Plus(l, r) =>
      val lenv = l(env)
      val renv = r(env)
      val left = backPropagate(env, lenv \/ (ran - renv), l)
      val right = backPropagate(env, renv \/ (ran - lenv), r)
      left \/ right
    case Multiply(l, r) if l == r =>
      val ransqrt = ran.sqrt
      val envl = l(env)
      (envl disjointFrom ransqrt, envl disjointFrom -ransqrt) match {
        case (false, true) => backPropagate(env, envl \/ ransqrt, l)
        case (true, false) => backPropagate(env, envl \/ (-ransqrt), l)
        case _             => backPropagate(env, envl \/ (ransqrt /\ (-ransqrt)), l)
      }
    case Multiply(Constant(x), e) =>
      val newran = if (x contains 0) e(env) else e(env) \/ (ran / x)
      backPropagate(env, newran, e)
    case Multiply(e, Constant(x)) =>
      val newran = if (x contains 0) e(env) else e(env) \/ (ran / x)
      backPropagate(env, newran, e)
    case Multiply(l, r) =>
      val lenv = l(env)
      val renv = r(env)
      if (lenv.contains(0) || renv.contains(0)) env
      else {
        lazy val left = backPropagate(env, lenv \/ (ran / renv), l)
        lazy val right = backPropagate(env, renv \/ (ran / lenv), r)
        left \/ right
      }
    case Divide(e, Constant(x)) => backPropagate(env, e(env) \/ (ran * x), e)
    case Divide(l, r) =>
      val lenv = l(env)
      val renv = r(env)
      val right = backPropagate(env, renv \/ (ran * renv), r)
      val left = backPropagate(env, lenv \/ (lenv / ran), l)
      left \/ right
  }

  def inverseRelation(env: Box, ran: Interval, arg: Arg, e: Expression): Interval = e match {
    case Plus(l, r) => arg match {
      case Left  => ran - r(env)
      case Right => ran - l(env)
    }
    case Multiply(l, r) =>
      arg match {
        case Left  => ran / r(env)
        case Right => ran / l(env)
      }
    case Divide(l, r) => arg match { // ran = l / r
      case Left  => ran * r(env) // l = ran * r
      case Right => l(env) / ran // r = l / ran
    }
  }

}

object ContractApp extends App with Contract {

  implicit val rnd = Parameters.default.rnd

  println(contract(Box("x" -> Interval(0, 2)), Interval(2), Multiply(Variable("x"), Variable("x"))))

} 
