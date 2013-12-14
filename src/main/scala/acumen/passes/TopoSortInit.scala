package acumen
package passes

import ASTUtil.exprSubParts

/* Pass to topologically sort the private section.  Used as part of
 * the "normalization" passes, but currently not enabled by
 * default. */

// FIXME: Need to do some error checking.  In particular if a variable
//   is defined in both the objects contractor (i.e. field) and the
//   priv section than the topo. sort could change the behavior

object TopoSortInit {
  case class CircularDep extends Errors.AcumenError {
    override def getMessage = 
      "Circular dependency detected in private section of some class."
  }

  def proc(p: Prog) : Prog = {
    Prog(p.defs.map{c => c.copy(priv = procPriv(c.priv))})
  }

  case class WithDeps(init: Init, deps: List[Name])
  def procPriv(priv: List[Init]) : List[Init] = {
    var withDeps = priv.map{init => WithDeps(init, getDeps(exprSubParts(init.rhs)))}
    withDeps = topoSort(Nil, withDeps)
    withDeps.map{_.init}
  }

  def topoSort(sorted: List[WithDeps], tosort: List[WithDeps]) : List[WithDeps] = {
    if (tosort.nonEmpty) {
      val sortedDeps = sorted.map{_.init.x}
      val (newSorted,stillTosort) = tosort.partition{_.deps.diff(sortedDeps).isEmpty}
      if (newSorted.isEmpty) throw CircularDep()
      topoSort(sorted ++ newSorted, stillTosort)
    } else {
      sorted
    }
  }

  def getDeps(es: List[Expr]) : List[Name] = {
    es.flatMap{e => e match {
      case Var(name) => List(name)
      case Dot(Var(Name("self", 0)), name) => List(name)
      case _ => getDeps(exprSubParts(e))
    }}.distinct
  }
  
}
