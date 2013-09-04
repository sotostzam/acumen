package acumen
package extract

import scala.collection.mutable.{ ListMap => MutListMap, ArrayBuffer, Stack => MutStack, ListBuffer, Map => MutMap}
import scala.util.control.Breaks.{ break, breakable }

/***************************************************************************
 * Data structures used for the extraction.  The act of forming these
 * data structures performs the SPLIT, TRANSFORM and FLATTEN steps
 ***************************************************************************/

trait IfTree[ActionT] extends Iterable[IfTree[ActionT]] {
  def id : Int
  def megId : Int

  def localConds : List[Cond]
  def localClaims : List[Cond]
  def conds : List[Cond] = if (parent != null) parent.conds ++ localConds else localConds
  def claims : List[Cond] = if (parent != null) parent.claims ++ localClaims else localClaims

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
               val localConds: List[Cond], val localClaims: List[Cond] = Nil) extends IfTree[Action] {
    // claims = "claims" used to annotate modes
    var children = new ArrayBuffer[Node]
    var contActions = new ArrayBuffer[ContinuousAction];
    var discrAssigns = new ArrayBuffer[Assign];
    var otherActions = new ArrayBuffer[Action]
    def actions : Seq[Action] = contActions.map{Continuously(_)} ++ discrAssigns.map{Discretely(_)} ++ otherActions
    val id : Int = {val res = idCounter; idCounter += 1; res}
    def node = this

    def toAST: IfThenElse =
      IfThenElse(Cond.toExpr(localConds),
                 children.map{_.toAST}.toList ++ actions, Nil)
    def dump = Pretty.pprint[Action](toAST) + "\n"

    def addChild(megId: Int, localConds: List[Cond], localClaims: List[Cond] = Nil) = {
      val if0 = new Node(this, megId, localConds, claims)
      children += if0
      if0
    }


    def contOnly : ContView = View(this, Parms(_.contActions, (n,v) => n.contActions = v, Set.empty))
    def discrOnly : DiscrView = View(this, Parms(_.discrAssigns, (n,v) => n.discrAssigns = v, Set.empty))

    def extractSimulatorAssigns(simulatorName: Name) : Seq[Assign] = {
      var (sim, non) = discrAssigns.partition {
        _ match {
          case Assign(Dot(Var(s), _), Lit(_)) if s == simulatorName => true
          case Assign(Dot(Dot(Var(Name("self", 0)), s), _), Lit(_)) if s == simulatorName => true
          case a @ _ => false
        }
      }
      discrAssigns = non
      sim
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

    // Return a new view that removes empty nodes while maintain the
    // invariants of the IfTree data structure
    def pruned : View[ActionT] = {
      val keep = MutMap.empty[Int,Boolean]
      def markKeeped(n: IfTree[ActionT]) : Unit = {
        n.children.foreach{markKeeped(_)}
        keep.getOrElse(n.id,{
          if (n.actions.nonEmpty) {
            // mark all siblings (including self) with the same megId
            if (n.parent != null) {
              n.parent.children.foreach{n2 => 
                if (n.megId == n2.megId) 
                  keep.update(n.id, true) }}
            // mark all parents
            var parent = n.parent 
            while (parent != null) {
              keep.update(parent.id, true); 
              parent = parent.parent
            }
          } else {
            // just mark self
            keep.update(n.id, false)
          }
        })
      }
      markKeeped(this)
      copy(p = p.copy(prune = keep.filter{case (_,v) => !v}.keySet))
    }
  }

  type ContView = View[ContinuousAction]
  type DiscrView = View[Assign]

  private var idCounter = 0;

  // Create an IfNode from a sequence of actions.
  def create(actions: Seq[Action]) : Node = {
    val root = new Node(null, 0, Nil)
    val megCounter = new {var idx = 1; 
                          def take() : Int = {val res = idx; idx += 1; res}}
    def populate(parent: Node, actions: Seq[Action]) : Unit = {
      actions.foreach {
        case IfThenElse(cond, ifTrue, ifFalse) =>
          val megId = megCounter.take()
          populate(parent.addChild(megId, List(Cond(cond))), ifTrue)
          populate(parent.addChild(megId, List(Cond.not(cond))), ifFalse)
        case Switch(subject, clauses) =>
          val megId = megCounter.take()
          clauses.foreach {
            case Clause(lhs, claim, actions) =>
              populate(parent.addChild(megId, List(Cond.eq(subject, lhs)), List(Cond(claim))), actions)
          }
          val switchNot = clauses.map { case Clause(lhs, _, _) => Cond.Not(Cond.eq(subject, lhs)) }
          parent.addChild(megId, switchNot)
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
    root
  }
}

