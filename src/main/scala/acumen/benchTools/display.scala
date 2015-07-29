/*
 * This file contains all the function using the terminal, used both for debugging
 *  and to be used next with the scripts to generate the plots
 */
package benchTool

import java.io._

object benchDisplay {
  type timeT = bench.timeT
  type LLtimeT = bench.LLtimeT
  type partialResultT = bench.partialResultT
  type ResultT = bench.ResultT
  type RefineResultT = bench.RefineResultT
  type trustResultT = bench.trustResultT

  def nbFormat(n: Double): String = "%1.4e\t".format(n)

  def displayWithMinQuartiles(total: RefineResultT, file: PrintWriter) = {
    val numberFormat = "%1.2e\t"
    for (tot <- total) {
      tot match {
        case (mod, inter, quartiles, a, b) => {
          file.println("Ran for " + a + " times and " + b + " seconds")
          file.println(mod + ", " + inter)
          file.println("STEP NAME\t\t\tMINIMUM\t\tLowerRange\tUpperRange\tLowerValue\tUpperValue\tSigma")
          val titles = List("Loading file\t\t",
            "Selecting interp.\t",
            "Parsing model\t\t",
            "SrcToSrc transf.\t",
            "Simulation\t\t\t",
            "Total\t\t\t\t")
          for (((min: timeT, lower: Double, upper), title: String) <- quartiles zip titles) {
            val sigma = upper - lower
            file.println(title + nbFormat(min) + nbFormat(lower)
              + nbFormat(upper) + nbFormat(min - 4 * sigma)
              + nbFormat(min + 4 * sigma) + nbFormat(sigma))
          }
        }
        case _ => throw new Exception("Wrong pattern")

      }
    }
    file.println()
  }

  def displayOneResult(result: partialResultT) = {
    result match {
      case (mod, inter, times, a, b) =>
        println("M:" + mod.split("/").last + " I:" + inter + times + "nbIte:" + a
          + "duration:" + b)
    }
  }
  def displayOneParsedExperiment(exp: ParsedExperimentType) = {
    exp match {

      case ParsedExperimentType(exec, inters, mods, (warmup, rep, opt)) =>
        opt match {
          case None => println("CLASSIC RUN")
          case Some((a, b)) => println("TABLE OF PRECISIONS (" + a + "," + b
            + ")")
        }
        println("Ways of running:" + exec)
        println("Interpreters:\t" + inters)
        println("Models:\t\t" + mods)
        println("Warmup:\t\t" + warmup + " times")
        println("Repetitions:\t" + rep + " times")
    }
  }

  def displayTrustResults(results: trustResultT) = {
    println("Avg iteration \tAvg time(ms)\tModel\t\t\tInterpreter\tGoodness(%)\tPrecision first\tPrecision mid\tPrecision last\tTotal time\tMax ind. time\tNumber of run")
    for (r <- results)
      r match {
        case (m, i, values, a, b, max) =>
          val totalTimes = values.map { x => x.last }
          val medOfMin = mathTools.quartiles(totalTimes.map { x => x._1 })._3
          val intervals = totalTimes.map {
            x =>
              val sigma = 8 * (x._2 + x._3);
              (x._1 - sigma, x._1 + sigma, sigma)
          }
          val nbOk = intervals.filter {
            x => x._1 < medOfMin && x._2 > medOfMin
          }.length
          val nbTot = values.length
          val goodness = if (nbTot <= 9) "Failed" else nbOk * 100 / nbTot
          print("%.1f\t\t".format(a.toDouble / nbTot))
          print("%.1f\t\t".format(b.toDouble / nbTot))
          print("%s\t".format(m.split("/").last))
          print("%s\t".format(i))
          print("%s\t\t".format(goodness))
          print("%.6f\t".format(intervals.head._3))
          print("%.6f\t".format(intervals(nbTot / 2)._3))
          print("%.6f\t".format(intervals.last._3))
          print("%.1f\t\t".format(b.toDouble))
          print("%.1f\t\t".format(max.toDouble))
          print("%d%n".format(nbTot))
      }
  }
}	
