package acumen
package extract

import scala.collection.mutable.{ ListMap => MutListMap, ArrayBuffer, Stack => MutStack, ListBuffer, Map => MutMap}
import scala.util.control.Breaks.{ break, breakable }
import CondImplicits._

/***************************************************************************
 * Data structures used for the extraction.  The act of forming these
 * data structures performs the SPLIT, TRANSFORM and FLATTEN steps
 ***************************************************************************/

trait IfTree[ActionT] extends Iterable[IfTree[ActionT]] {
  def id : Int
  def megId : Int

  def localConds : Cond
  def localClaims : Cond
  def conds : Cond = if (parent != null) Cond.and(parent.conds,localConds) else localConds
  def claims : Cond = if (parent != null) Cond.and(parent.claims, localClaims) else localClaims

  def actions : Seq[ActionT]

  def parent : IfTree[ActionT]
  def children : Seq[IfTree[ActionT]]

  def node : IfTree.Node

  def iterator = {
    val root = this
    new Iterator[IfTree[ActionT]] {
      val stack = MutStack[IfTree[ActionT]](root)
      def next() = {
        val top = stack.pop()
        stack.pushAll(top.children.reverse)
        top
      }
      def hasNext = stack.nonEmpty
    }
  }
}

object IfTree {
  class Node(val parent: Node, val megId: Int, // meg = mutely exclusive id
             val localConds: Cond, val localClaims: Cond = Cond.True) extends IfTree[Action] {
    // claims = "claims" used to annotate modes
    var children = new ArrayBuffer[Node]
    var contActions = new ArrayBuffer[ContinuousAction];
    var discrAssigns = new ArrayBuffer[Assign];
    var otherActions = new ArrayBuffer[Action]
    def actions : Seq[Action] = contActions.map{Continuously(_)} ++ discrAssigns.map{Discretely(_)} ++ otherActions
    val id : Int = {val res = idCounter; idCounter += 1; res}
    def node = this

    def toAST: IfThenElse =
      IfThenElse(localConds.toExpr,
                 children.map{_.toAST}.toList ++ actions, Nil)
    def dump = Pretty.pprint[Action](toAST) + "\n"
    
    def addChild(megId: Int, localConds: Cond, localClaims: Cond = Cond.True) = {
      val if0 = new Node(this, megId, localConds, localClaims)
      children += if0
      if0
    }


    def contOnly : ContView = View(this, Parms(_.contActions, (n,v) => n.contActions = v, Set.empty))
    def discrOnly : DiscrView = View(this, Parms(_.discrAssigns, (n,v) => n.discrAssigns = v, Set.empty))

    def extractSimulatorAssigns(simulatorName: Name) : Seq[Assign] = {
      val (sim, non) = Util.getSimulatorAssigns(simulatorName, discrAssigns)
      discrAssigns = non
      sim
    }

    def copy(parent: Node) : Node = {
      val that = new Node(parent, this.megId, 
                          this.localConds, this.localClaims)
      that.children ++= this.children.map{_.copy(that)}
      that.contActions ++= this.contActions
      that.discrAssigns ++= this.discrAssigns
      that.otherActions ++= this.otherActions
      that
    }

    def validate() {
      children.foreach{n =>
        n.validate()
        assert(n.parent == this)
      }
    }
  }

  case class Parms[ActionT](get: Node => ArrayBuffer[ActionT],
                            put: (Node, ArrayBuffer[ActionT]) => Unit,
                            prune: collection.Set[Int])
  case class View[ActionT](val node: Node, val p: Parms[ActionT]) extends IfTree[ActionT] {
    def parent : View[ActionT] = if (node.parent != null) View(node.parent,p) else null
    def megId = node.megId
    def localConds = node.localConds
    def localClaims = node.localClaims
    def children : Seq[View[ActionT]] = node.children.filter{v => !p.prune.contains(v.id)}.map{View(_,p)}
    def actions = p.get(node)
    def actions_=(v: ArrayBuffer[ActionT]) = p.put(node, v)
    def id = node.id

    def withPruneSet(prune2: collection.Set[Int]) = copy(p=p.copy(prune=prune2))

    // Return a new view that removes empty nodes while maintain the
    // invariants of the IfTree data structure
    def pruned : View[ActionT] = {
      val keep = MutMap.empty[Int,Boolean]
      def markKeeped(n: IfTree[ActionT]) : Unit = {
        n.children.foreach{markKeeped(_)}
        val megIdsToKeep = n.children.filter{n2 => keep(n2.id)}.map{_.megId}.distinct
        n.children.foreach{n2 => if (megIdsToKeep.contains(n2.megId)) keep(n2.id) = true}
        keep(n.id) = n.actions.nonEmpty || megIdsToKeep.nonEmpty
      }
      markKeeped(this)
      val prune = keep.filter{case (_,v) => !v}.keySet
      withPruneSet(prune)
    }
  }

  type ContView = View[ContinuousAction]
  type DiscrView = View[Assign]

  private var idCounter = 0;

  // Create an IfNode from a sequence of actions.
  def create(actions: Seq[Action]) : Node = {
    val root = new Node(null, 0, Cond.True)
    def populate(parent: Node, actions: Seq[Action]) : Unit = {
      actions.foreach {
        case IfThenElse(cond, ifTrue, ifFalse) =>
          val megId = idCounter
          populate(parent.addChild(megId, Cond(cond)), ifTrue)
          populate(parent.addChild(megId, Cond.not(cond)), ifFalse)
        case Switch(subject, clauses) =>
          val megId = idCounter
          clauses.foreach {
            case Clause(lhs, claim, actions) =>
              populate(parent.addChild(megId, Cond.eq(subject, lhs), Cond(claim)), actions)
          }
          val switchNot = clauses.map { case Clause(lhs, _, _) => Cond.Not(Cond.eq(subject, lhs)) }
          //parent.addChild(megId, switchNot)
          // FIMXE^: This is an error in acumen, need some to some how make it an error... - kevina
        case Continuously(a) =>
          parent.contActions += a
        case Discretely(a:Assign) =>
          parent.discrAssigns += a
        case a =>
          parent.otherActions += a
      }
    }
    populate(root,actions)
    root.validate()
    root
  }

  def mergeNodes(nodes: Seq[Node], megId: Int) : Node = {
    val parents = nodes.map{_.parent}.groupBy{_.id}
    assert(parents.size == 1)
    val parent = parents.values.head.head
    val res = new Node(parent, megId, nodes.map{_.localConds}, nodes.map{_.localClaims})
    nodes.foreach{n => 
      res.children ++= n.children.map{_.copy(res)}
      res.contActions ++= n.contActions
      res.discrAssigns ++= n.discrAssigns
      res.otherActions ++= n.otherActions
    }
    res.validate()
    res
  }
}

