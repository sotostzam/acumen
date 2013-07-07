package acumen
package interpreters
package imperative

import scala.collection.immutable.HashMap

import acumen.Errors._
import acumen.Pretty._
import acumen.util.Conversions._
import acumen.util.Random
import acumen.interpreters.Common.{ classDef, evalOp }
import acumen.util.Canonical.{
  childrenOf, 
  classf,
  cmain,
  cmagic,
  endTime,
  nextChild,
  parentOf,
  parent,
  seedOf,
  seed1,
  seed2,
  self,
  resultType,
  time,
  timeStep
}
import scala.annotation.tailrec

class ImperativeInterpreter extends CStoreInterpreter {
  import Common._

  type Store = Common.Store
  def repr (s:Store) : CStore = Common.repr(s)
  def fromCStore (cs:CStore, root:CId) : Store = Common.fromCStore(cs, root)

  var cstoreOpts = new CStoreOpts

  def init(prog: Prog, opts: CStoreOpts): (Prog, Store) = {
    cstoreOpts = opts
    val magic = fromCStore(magicCObj, CId(0))
    /* WARNING: the following line works because there is no children access check
       if one of the instructions of the provate section tries to access magic,
       and there was a check, this would crash (which we don't want) */
    val (sd1, sd2) = Random.split(Random.mkGen(0))
    val mainObj = mkObj(cmain, prog, None, sd1, List(VObjId(Some(magic))), magic, 1)
    magic.seed = sd2
    changeParent(magic, mainObj)
    val cprog = CleanParameters.run(prog, CStoreInterpreterType)
    val sprog = Simplifier.run(cprog)
    val mprog = Prog(magicClass :: sprog.defs)
    (mprog , mainObj)
  }

  def step(p: Prog, st: Store): Option[Store] = {
    val magic = getSimulator(st)
    if (getTime(magic) > getEndTime(magic)) {
      None
    } else {
      @tailrec def step0() : Unit = {
        stepInit
        if (getTime(magic) > getEndTime(magic))
          return
        val chtset = traverse(evalStep(p, magic), st)
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
                if (!cstoreOpts.outputSomeDiscrete) return step0()
              case NoChange() =>
                setResultType(magic, FixedPoint)
                if (!cstoreOpts.outputAllRows) return step0()
            }
          case FixedPoint =>
            setResultType(magic, Continuous)
            setTime(magic, getTime(magic) + getTimeStep(magic))
            if (cstoreOpts.outputLastOnly) return step0()
        }
      }
      step0()
      Some(st)
    }
  }
  def stepInit : Unit = {}
  def traverse(f: ObjId => Changeset, root: ObjId): Changeset =
    traverseSimple(f, root)

}

// The ImperativeInterpreter is stateless hence it is okay to have a
// single global instance
// FIXME: Make the above statement true ...
object ImperativeInterpreter extends ImperativeInterpreter
