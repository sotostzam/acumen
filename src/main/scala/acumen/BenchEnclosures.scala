package acumen

import Errors._
import Pretty._
import util.Filters._
import Ordering.Implicits._
import acumen.interpreters._
import collection._
import collection.mutable.MutableList

object BenchEnclosures {

  val REPEAT = 3

  def run(i: Interpreter, prog: Prog, args: Array[String], argsOffset : Int) {
    val prefix = args(argsOffset)
    val parms = getParms(args.slice(argsOffset + 1, Int.MaxValue))
    var trials = flatten(cartesianProduct(parms))
    trials = duplicate(3, trials)
    trials = scala.util.Random.shuffle(trials)
    val ie = i.asInstanceOf[enclosure.Interpreter]
    //val res = trials.map{el: List[(String, Double)] => 
    //  (el.map{(_._2)}.toArray, new Array[Double](3))}.toMap
    var res = new mutable.ListMap[String,MutableList[(List[(String, Double)],Double)]] {
      def add(key: String, adjs: List[(String, Double)], v: Double) = 
        getOrElseUpdate(key,MutableList[(List[(String, Double)],Double)]()) += ((adjs,v))
    }
    println("Prepping with default parms.")
    ie.run(prog)
    println("Prep done, time irrelevant")
    for (adjustments <- trials) {
      println("===")
      def adjustParms(p: acumen.interpreters.enclosure.Parameters) =
        adjustments.foldLeft(p){case (p,(k,v)) => k match {
          case "minTimeStep"    => p.copy(minTimeStep = v)
          case "maxTimeStep"    => p.copy(maxTimeStep = v)
          case "minImprovement" => p.copy(minImprovement = v)
          case what              => throw new Error("Unknown parm: " + what)
        }}
      println("Starting with parms: " + adjustments)
      val s = System.currentTimeMillis
      var r = try {
        ie.runInterpreter(prog,ie.defaultInterpreterCallbacks,adjustParms)
      } catch {
        case e => println(e); null
      }
      if (r != null) {
        val time = (System.currentTimeMillis - s)/1000.0
        println("Time to run simulation: %f".format(time))
        res.add("runtime",adjustments, time)
        r.printLast
        implicit val rnd = enclosure.Rounding(10)
        val e = r.res.last
        res.add("precision-norm",adjustments, enclosure.Types.norm(e(e.domain.high)).hiDouble)
        for (v <- e.varNames) {
          val ev = e(v)
          res.add("precision-" + v, adjustments, ev(ev.domain.high).width.hiDouble)
        }
      }
    }
    for (what <- res.keys) {
      val grouped = res(what).groupBy{_._1.map{_._2}.toSeq}.mapValues{_.map{_._2}.toArray.sorted}
      println("===")
      val fn = prefix + "-" + what + ".dat"
      val out =  new java.io.PrintWriter(new java.io.FileWriter(fn))
      out.println("# Args: " + args.mkString(" "))
      out.println(parms.map{_._1}.flatten.mkString(" ") + " : " +
                  "avg sd : " +
                  "raw_data_sorted " + Stream.fill(REPEAT-1)("-").toList.mkString(" "))
      for (adjustments <- grouped.keys.toList.sorted) {
        val vals = grouped(adjustments)
        val avg = vals.sum / vals.length
        val sd = math.sqrt(vals.map{x => math.pow(x - avg,2)}.sum / (vals.length - 1))
        out.println(adjustments.map{_.toString}.mkString(" ") + " : " + 
                    avg + " +- " + sd + " : " +
                    vals.map{_.toString}.mkString(" "))
      }
      out.close
      val in = new java.io.BufferedReader(new java.io.FileReader(fn));
      println("RESULTS from " + fn)
      var line : String = null
      while ({line = in.readLine; line != null})
      println(line)
      in.close
    }
  }

  def getParms(args: Array[String]) : List[(Array[String], List[Array[Double]])] = 
    args.map{arg: String => val name :: vals :: nil = arg.split("=").toList
                            (name.split(":"), 
                             vals.split(",").toList.map{_.split(":").map{_.toDouble}})}
      .toList

  def cartesianProduct(parms: List[(Array[String], List[Array[Double]])]) : List[List[(Array[String], Array[Double])]] = {
    def step(ps: List[(Array[String], List[Array[Double]])]) : List[List[(Array[String], Array[Double])]] = ps match {
      case (key, vals) :: rest => product(vals.map{v => (key, v)}, step(rest))
      case what                => List(Nil)
    }
    def product(heads: List[(Array[String], Array[Double])], tails: List[List[(Array[String], Array[Double])]]) = {
      val res = for (head <- heads; tail <- tails) yield head :: tail
      res
    }
    step(parms)
  }

  def flatten(trials: List[List[(Array[String], Array[Double])]]) : List[List[(String, Double)]] =
    trials.map{adjs : List[(Array[String], Array[Double])] => 
      var res : List[(String, Double)] = Nil
      for ((key,v) <- adjs) {
        for (idx <- key.indices) {
          res = (key(idx), v(idx)) :: res
        }
      }
      res.reverse
    }
  
  def duplicate(times: Int, trials: List[List[(String, Double)]]) : List[List[(String, Double)]] = 
    if (times <= 1) trials
    else trials ++ duplicate(times -1, trials)

}
