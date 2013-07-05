package acumen.interpreters.imperative.sequential

import scala.math._
import acumen.Errors._
import acumen.Pretty._
import acumen.util.Names._
import acumen.util.Conversions._
import acumen.util.Canonical._
import acumen.{Continuous, Discrete, Prog}

import scala.annotation.tailrec

class Interpreter extends acumen.CStoreInterpreter {
  val I = acumen.interpreters.imperative.sequential.Interpreter
  import I._
  import acumen._

  type Store = I.Store
  def init(prog: Prog, opts: CStoreOpts) = {I.init(prog,opts)}
  def fromCStore(st: CStore, root: CId) = I.fromCStore(st, root)
  def repr(st: Store) = I.repr(st)
  
  def step(p: Prog, st: Store): Option[Store] = {
    val magic = getSimulator(st)
    if (getTime(magic) > getEndTime(magic)) {
      None
    } else {
      @tailrec def step0() : Unit = {
        if (getTime(magic) > getEndTime(magic))
          return
        val chtset = iterateSimple(evalStep(p, magic), st)
        getResultType(magic) match {
          case Discrete | Continuous =>
            chtset match {
              case SomeChange(dead, rps) =>
                for ((o, p) <- rps)
                  changeParent(o, p)
                for (o <- dead) {
                  o.parent match {
                    case None => ()
                    case Some(op) =>
                      for (oc <- o.children) changeParent(oc, op)
                      op.children = op.children diff Seq(o)
                  }
                }
                setResultType(magic, Discrete)
                if (!I.cstoreOpts.outputSomeDiscrete) return step0()
              case NoChange() =>
                setResultType(magic, FixedPoint)
                if (!I.cstoreOpts.outputAllRows) return step0()
            }
          case FixedPoint =>
            setResultType(magic, Continuous)
            setTime(magic, getTime(magic) + getTimeStep(magic))
            if (I.cstoreOpts.outputLastOnly) return step0()
        }
      }
      step0()
      Some(st)
    }
  }

  def iterateSimple(f: ObjId => Changeset, root: ObjId): Changeset = {
    val r = f(root)
    val cs = root.children
    r || combine(cs, iterateSimple(f, _: ObjId))
  }
}

object Interpreter extends acumen.interpreters.imperative.Common {

  def withInterpreter[A](f: Interpreter => A): A = {
    val pi = new Interpreter()
    f(pi)
  }

}
