package acumen
package interpreters
package reference2015

import Interpreter._
import Common._
import acumen.Errors._

/** Used to represent the statements that are active at a given point during the simulation. */
case class Changeset
  ( born: List[CollectedCreate]     = Nil /* born */
  , dead: List[CId]                 = Nil /* dead */
  , reps: List[(CId,CId)]           = Nil /* reparentings */
  , das:  List[CollectedAction]     = Nil /* discrete assignments */
  , eqs:  List[CollectedAction]     = Nil /* continuous assignments / equations */
  , odes: List[CollectedAction]     = Nil /* ode assignments / differential equations */
  , hyps: List[CollectedHypothesis] = Nil /* hypotheses */
  ) {
  def ++(that: Changeset) =
    Changeset(born ++ that.born, dead ++ that.dead, reps ++ that.reps, das ++ that.das, eqs ++ that.eqs, odes ++ that.odes, hyps ++ that.hyps)
}
object Changeset {
  val empty = Changeset()
}

case class CollectedCreate(da: Option[(CId, Name)], c: ClassName, parent: CId, sd: (Int, Int), ves: List[CValue])
case class CollectedAction(o: CId, d: Index, rhs: Expr, env: Env)
case class CollectedHypothesis(o: CId, s: Option[String], h: Expr, env: Env)

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
    
  def logBorn(da: Option[(CId, Name)], c: ClassName, parent: CId, sd: (Int, Int), ves: List[CValue]) : Eval[Unit] = 
    mkEval(s => ((), Changeset(born = List(CollectedCreate(da, c, parent, sd, ves))), s))   
 
  def logDead(id:CId) : Eval[Unit] = 
    mkEval(s => ((), Changeset(dead = List(id)), s))

  def logReparent(o:CId, parent:CId) : Eval[Unit] =
    mkEval(s => ((), Changeset(reps = List((o,parent))), s))
    
  def logAssign(o: CId, d: Index, r: Expr, e: Env) : Eval[Unit] =
    mkEval(s => ((), Changeset(das = List(CollectedAction(o,d,r,e))), s))

  def logEquation(o: CId, d: Index, r: Expr, e: Env) : Eval[Unit] =
    mkEval(s => ((), Changeset(eqs = List(CollectedAction(o,d,r,e))), s))

  def logODE(o: CId, d: Index, r: Expr, e: Env) : Eval[Unit] =
    mkEval(s => ((), Changeset(odes = List(CollectedAction(o,d,r,e))), s))

  def logHypothesis(o: CId, n: Option[String], h: Expr, e: Env) : Eval[Unit] =
    mkEval(s => ((), Changeset(hyps = List(CollectedHypothesis(o,n,h,e))), s))

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
