package acumen
package interpreters
package reference2014

import Interpreter._
import acumen.Errors._

/** Used to represent the statements that are active at a given point during the simulation. */
case class Changeset
  ( dead: List[CId]               = Nil /* dead */
  , reps: List[(CId,CId)]         = Nil /* reparentings */
  , das:  List[DelayedAction]     = Nil /* discrete assignments */
  , eqs:  List[DelayedAction]     = Nil /* continuous assignments / equations */
  , odes: List[DelayedAction]     = Nil /* ode assignments / differential equations */
  , hyps: List[DelayedHypothesis] = Nil /* hypotheses */
  ) {
  def ++(that: Changeset) =
    Changeset(dead ++ that.dead, reps ++ that.reps, das ++ that.das, eqs ++ that.eqs, odes ++ that.odes, hyps ++ that.hyps)
}
object Changeset {
  val empty = Changeset()
}

case class DelayedAction(o: CId, d: Dot, rhs: Expr, env: Env)
case class DelayedHypothesis(o: CId, s: Option[String], h: Expr, env: Env)

/** A custom state+writer monad, inpired by the state monad of scalaz. */
sealed trait Eval[+A] {
  import Eval._

  def apply(s: Store) : 
    ( A         /* result */ 
    , Changeset /* (writer part) */
    , Store     /* current store (state part) */
    )

  def map[B](f: A => B) : Eval[B] = mkEval (
    apply(_) match { case (a, cs, st) => (f(a), cs, st) }
  )

  def foreach[B](f: A => Eval[B]) : Eval[B] = flatMap(f)

  def flatMap[B](f: A => Eval[B]) : Eval[B] = mkEval (
    apply(_) match { 
      case (a, cs, st) => 
        val (a1, cs1, st1) = f(a)(st)
        (a1, cs ++ cs1, st1)
    }
  )
  
  def >>[B](m: Eval[B]) = this flatMap (_ => m)
  def >>=[B](f: A => Eval[B]) : Eval[B] = flatMap(f)
  
  def filter(p: A => Boolean) : Eval[A] = mkEval (
    apply(_) match { case t@(a, _, _) =>
      if (p(a)) t else throw ShouldNeverHappen() 
    } 
  )

  def !  (s: Store) : A = apply(s)._1
  def ~> (s:Store) : Store = apply(s)._3

}

object Eval {

  def mkEval[A](f: Store => (A, Changeset, Store)) : Eval[A] = 
	  new Eval[A] { def apply(s:Store) = f(s) }
 
  /** Used to inject write operations of 'util.Canonical' into the monad. */
  def promote(f: Store => Store) : Eval[Unit] =
    mkEval (s => ((), Changeset.empty, f(s)))

  def pure[A](x:A) : Eval[A] = mkEval(s => (x, Changeset.empty, s))
  def pass = pure(())
  
  def getStore : Eval[Store] = mkEval(s => (s, Changeset.empty, s))
  def modifyStore(f:Store => Store) : Eval[Unit] = 
    mkEval(s => ((), Changeset.empty, f(s)))

  def logCId(id:CId) : Eval[Unit] = 
    mkEval(s => ((), Changeset(dead = List(id)), s))

  def logReparent(o:CId, parent:CId) : Eval[Unit] =
    mkEval(s => ((), Changeset(reps = List((o,parent))), s))
    
  def logAssign(o: CId, d: Dot, r: Expr, e: Env) : Eval[Unit] =
    mkEval(s => ((), Changeset(das = List(DelayedAction(o,d,r,e))), s))

  def logEquation(o: CId, d: Dot, r: Expr, e: Env) : Eval[Unit] =
    mkEval(s => ((), Changeset(eqs = List(DelayedAction(o,d,r,e))), s))

  def logODE(o: CId, d: Dot, r: Expr, e: Env) : Eval[Unit] =
    mkEval(s => ((), Changeset(odes = List(DelayedAction(o,d,r,e))), s))

  def logHypothesis(o: CId, n: Option[String], h: Expr, e: Env) : Eval[Unit] =
    mkEval(s => ((), Changeset(hyps = List(DelayedHypothesis(o,n,h,e))), s))

  /** Apply f to the store and wrap it in an Eval */
  def asks[A](f : Store => A) : Eval[A] = 
    getStore >>= ((s:Store) => pure(f(s)))
  
  def sequence[A](es:List[Eval[A]]) : Eval[List[A]] = es match {
    case Nil     => pure(Nil)
    case (e::es) => for (x  <- e; xs <- sequence(es)) yield x::xs 
  }
  def sequence_[A](es:List[Eval[A]]) : Eval[Unit] =
    es match {
      case Nil     => pure(())
      case (e::es) => e >> sequence_(es)  
    }
  
  def mapM[A,B](f:A => Eval[B], xs:List[A]) : Eval[List[B]] = 
    sequence(xs map f)
  def mapM_[A,B](f:A => Eval[B], xs:List[A]) : Eval[Unit] = 
    sequence_(xs map f)

} 
