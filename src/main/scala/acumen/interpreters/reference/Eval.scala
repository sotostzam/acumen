package acumen
package interpreters
package reference

import Interpreter._
import acumen.Errors._
import scala.collection.immutable.Set

/* a custom state+writer monad, inpired by the state monad of scalaz */

sealed trait Eval[+A] {
  import Eval._

  def apply(s: Store) : 
    (A /* result */, 
     Set[CId] /* dead (writer part) */, 
     Set[(CId,CId)] /* reparentings (writer part) */,
     Store /* current store (state part) */)

  def map[B](f: A => B) : Eval[B] = mkEval (
    apply(_) match { case (a, ids, rps, st) => (f(a), ids, rps, st) }
  )

  def foreach[B](f: A => Eval[B]) : Eval[B] = flatMap(f)

  def flatMap[B](f: A => Eval[B]) : Eval[B] = mkEval (
    apply(_) match { 
      case (a, ids, rps, st) => 
        val (a1, ids1, rps1, st1) = f(a)(st)
        (a1, ids ++ ids1, rps ++ rps1, st1)
    }
  )
  
  def >>[B](m: Eval[B]) = this flatMap (_ => m)
  def >>=[B](f: A => Eval[B]) : Eval[B] = flatMap(f)
  
  def filter(p: A => Boolean) : Eval[A] = mkEval (
    apply(_) match { case t@(a, _, _, _) =>
      if (p(a)) t else throw ShouldNeverHappen() 
    } 
  )

  def !  (s: Store) : A = apply(s)._1
  def ~> (s:Store) : Store = apply(s)._4

}

object Eval {

  def mkEval[A](f: Store => (A, Set[CId], Set[(CId,CId)], Store)) : Eval[A] = 
	new Eval[A] { def apply(s:Store) = f(s) }
 
  /* used to inject write operations of 'util.Canonical' into the monad */
  def promote(f: Store => Store) : Eval[Unit] =
    mkEval (s => ((), Set.empty, Set.empty, f(s)))

  def pure[A](x:A) : Eval[A] = mkEval(s => (x, Set.empty, Set.empty, s))
  def pass = pure(())
  
  def getStore : Eval[Store] = mkEval(s => (s, Set.empty, Set.empty, s))
  def modifyStore(f:Store => Store) : Eval[Unit] = 
    mkEval(s => ((), Set.empty, Set.empty, f(s)))

  def logCId(id:CId) : Eval[Unit] = 
    mkEval (s => ((), Set(id), Set.empty, s))

  def logReparent(o:CId, parent:CId) : Eval[Unit] =
    mkEval(s => ((), Set.empty, Set((o,parent)), s))
  
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

  def foldLeftM[A,B](a:A, f:(A,B) => Eval[A], l:List[B]) : Eval[A] =
    l match {
      case Nil     => pure(a)
      case (x::xs) => f(a,x) >>= (a1 => foldLeftM(a1,f,xs))
    }

} 
