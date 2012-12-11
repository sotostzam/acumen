val testExp = new EPlus(new EVar("a"), new EVar("i"))
val testExp2 = new ERepeat("i", 1, 1000, "a", 0, testExp)
var testEnv = new Env

println("Running Repeat(i, 1, 100, a, 0, a+1)")
println(testExp2.meaning(testEnv))
 
class Env {
  var env = (n: String) => 99999999
  def lookup(name: String): Int = env(name)
  def extend(name: String, value: Int): Env = {
    val oldEnv = env
    env = (n: String) => if(n == name) value else oldEnv(n)
    this
  }
}

trait Exp {
  def meaning(env: Env): Int
}

class EInt(value: Int) extends Exp {
  override def meaning(env: Env): Int = value
}

class EVar(name: String) extends Exp {
  override def meaning(env: Env): Int =
    env.lookup(name)
}

class EPlus(e1: Exp, e2: Exp) extends Exp {
  override def meaning(env: Env): Int =
    e1.meaning(env) + e2.meaning(env)
}

class EMinus(e1: Exp, e2: Exp) extends Exp {
  override def meaning(env: Env): Int =
  e1.meaning(env) - e2.meaning(env)
}

class ERepeat(counter: String, start: Int, end: Int, accum: String, accum_init: Int, accum_exp: Exp) extends Exp {
  override def meaning(env: Env): Int = {
    if(start > end)
      accum_init
    else {
      val startp = start + 1
      val envp = env.extend(counter, start).extend(accum, accum_init)
      val accum_initp = accum_exp.meaning(envp)
      (new ERepeat(counter, startp, end, accum, accum_initp, accum_exp)).meaning(envp)
    }
  }
}
