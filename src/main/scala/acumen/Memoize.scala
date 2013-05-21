package acumen

trait Memoize[A, B] {

  /**
   * The expression context. Used to store bindings of expressions to
   * variable names, keyed by the expressions themselves.
   */
  private val ctx = new scala.collection.mutable.HashMap[A, B]()

  /**
   * Memoization funciton, used for hash-consing of subexpressions.
   * This function is called by the smart constructors and helps to
   * avoid that duplicates of subexpressions are stored in memory.
   * The resulting structure of references on the heap can be used
   * to build a reduced expression using a series of let-statements
   * corresponding to the hash-consed subexpressions.
   */
  def mem(f: A => B)(a: A): B =
    ctx.get(a) match {
      /* If the expression already exists in the context, return the 
       * cached reference.
       */
      case Some(b) => b
      /* Otherwise save a reference to the expression e in the context 
       * ctx along with a unique variable name x_i, where i is the 
       * index of the expression in the context.
       */
      case _       => { val b = f(a); ctx += (a -> b); b }
    }

  def values = ctx.values
  
}