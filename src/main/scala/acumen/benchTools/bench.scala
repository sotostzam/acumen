/*
 * This is the main file of the tool: hold "runTimer" the called function, 
 * the type definitions and the calls for others parts of the tool
 */
package benchTool

import acumen._
import java.io._
import java.nio._
import scala.annotation.tailrec

//Object to open/read a file
object IO {
  def openOutputFile(file: String): PrintWriter = {
    new File(file).getAbsoluteFile.getParentFile().mkdirs()
    new PrintWriter(file, "UTF-8")
  }
  def readInputFile(file: String): String = {
    val fileObject = new File(file)
    val filePath = fileObject.getAbsoluteFile
    scala.io.Source.fromFile(filePath).mkString
  }
}

object bench {
  type timeT = Double //one time value
  //times for a run (one execution of one model with one interpreter)
  type LtimeT = List[timeT]
  type LLtimeT = List[LtimeT] //times for different runs
  type partialResultT = (String, String, LLtimeT, Int, Long)
  type ResultT = List[partialResultT]

  type minAndSpread = List[(timeT, timeT, timeT)]

  /*
   * Types for the results once the mathematical function have been applied
   */
  type partialRefineResultT = (String, String, minAndSpread, Int, Long)
  type RefineResultT = List[partialRefineResultT]
  type LRefineResultT = List[RefineResultT]

  /*
   * Types for statistical results
   */
  type partialTrustResultT = (String, String, List[minAndSpread], Int, Long, Long)
  type trustResultT = List[partialTrustResultT]

  /**
   * Core function to measure times for a model and an interpreter
   */
  def timeModInter(model: String, inter: String): (LtimeT) = {
    val t0 = System.currentTimeMillis
    val file = new File(model).getAbsoluteFile
    def in = new InputStreamReader(Main.insertParamModel(file))
    val t1 = System.currentTimeMillis
    val semantics = SemanticsImpl(inter)
    val i = semantics.interpreter().asInstanceOf[CStoreInterpreter]
    val t3 = System.currentTimeMillis
    val ast = semantics.parse(in, file.getParentFile(), Some(file.getName()))
    val t4 = System.currentTimeMillis
    val final_out = semantics.applyPasses(ast, Seq.empty[String])
    val t5 = System.currentTimeMillis
    SingleStep.done = false // Re-set global state! 
    val history = i.run(final_out) match { case CStoreRes(r, _) => r }
    history.last // Force evaluation of the history stream
    val t6 = System.currentTimeMillis
    List(t6 - t5, t6 - t0)
    List(t1 - t0, t3 - t1, t4 - t3, t5 - t4, t6 - t5, t6 - t0)
  }

  /**
   *  standard way to run the measurements : return the quartiles and the minimum
   *  for each pair (model, interpreter)
   */
  def runExperiment(exps: ParsedExperimentType,
                    writer: PrintWriter): LRefineResultT = {
    exps match {
      case ParsedExperimentType(executions, inters, mods, (warmup, repeat, opt)) =>
        (for (rep <- 0 to repeat; e <- executions) yield {
          val rawValues: ResultT =
            e match {
              case Single(nbIte, time) =>
                SingleRun.run(mods, inters, time, nbIte)
              case Interleaved(nbIte, time) =>
                InterleavedRun.run(mods, inters, time, nbIte)
            }
          val values: RefineResultT = mathTools.applyMinAndQuartiles(rawValues)
          values
        }).toList
    }
  }

  /**
   * Run the experiment but return full statistics for the results :
   *  goodness, global time, average time, time for some executions....
   */
  def runPrecisedExperiment(exps: ParsedExperimentType, minNbIte: Int,
                            minDurationSec: Double, writer: PrintWriter): Unit = {
    val tStart = System.currentTimeMillis
    val finalRes = exps match {
      case ParsedExperimentType(exec, inters, mods, (warmup, rep, opt)) =>
        precisedExperimentAux(exec, inters, mods, minNbIte, 1000 * minDurationSec + tStart, 0, Nil)
    }
    benchDisplay.displayTrustResults(finalRes)
  }

  /**
   * tail recursive function to generate all intermediary results.
   */
  @tailrec def precisedExperimentAux(exec: List[ExecutionType], inters: List[String],
                                     mods: List[String], minNbIte: Int, timeToEnd: Double, iteNumber: Int,
                                     tmpGoodnessRes: trustResultT): trustResultT = {
    if (iteNumber < minNbIte && System.currentTimeMillis < timeToEnd) {
      println("Iteration " + iteNumber)
      val results = (for (e <- exec) yield {
        val rawResult = e match {
          case Single(nbIte, time) =>
            SingleRun.run(mods, inters, time, nbIte)
          case Interleaved(nbIte, time) =>
            InterleavedRun.run(mods, inters, time, nbIte)
        }
        val modifiedResult = mathTools.applyMinAndQuartiles(rawResult)
        modifiedResult.map { x => toPartialTrust(x) }
      }).toList.flatten
      precisedExperimentAux(exec, inters, mods, minNbIte, timeToEnd, iteNumber + 1,
        compress(results ::: tmpGoodnessRes))
    } else tmpGoodnessRes
  }

  /**
   * Convert a partialRefineResultT object into a partialTrustResultT object
   */
  def toPartialTrust(refRes: partialRefineResultT): partialTrustResultT = {
    refRes match {
      case (mod, inter, quartiles, nbIte, duration) =>
        (mod, inter, List(quartiles), nbIte, duration, duration)
    }
  }

  /**
   * Merge two partialTrustResultT objects
   */
  def mergeTrustResult(r1: partialTrustResultT,
                       r2: partialTrustResultT): partialTrustResultT = {
    (r1, r2) match {
      case ((mod, inter, quartList, nbIte, duration, max),
        (mod2, inter2, quartList2, nbIte2, duration2, max2)) if (mod == mod2 && inter == inter2) =>
        (mod, inter, quartList ::: quartList2, nbIte + nbIte2, duration + duration2,
          if (max > max2) max else max2)
    }
  }

  /**
   * Remove duplicated pairs of model-interpreter
   */
  def compress(list: trustResultT): trustResultT = {
    val groupedList = list.groupBy(res => res match {
      case (mod, inter, quartiles, a, b, max) =>
        (mod, inter)
    })
    (for ((key, value) <- groupedList) yield key match {
      case (mod, inter) =>
        value.foldLeft((mod, inter, Nil: List[minAndSpread], 0, 0L, 0L))(mergeTrustResult)
    }).toList
  }

  /* 
   * Entry point of the library
   */
  def runTimer(input: String, output: String): Unit = {
    //File creation
    val inputLines = IO.readInputFile(input)
    val writer = IO.openOutputFile(output)
    writer.println(new java.util.Date())
    //Parameter setting
    val splitedInputLines = "GNUPLOT".r.split(inputLines)
    val ParsedExperimentType(exec, inters, mods, (warmup, rep, opt)) =
      ExperimentParser.parse(splitedInputLines(0))

    //Warm up step
    for (i <- 1 to warmup) {
      timeModInter("examples/01_CPS_Course/01_Introduction/01_Geometry/02_text.acm",
        "optimized2015")
    }
    writer.println("Warmup done " + warmup + " times")
    writer.println()

    //Measurements depending on the type of experiment the user asked for
    opt match {
      case None =>
        val results = runExperiment(
          ParsedExperimentType(exec, inters, mods, (warmup, rep, opt)), writer)
        for (res <- results)
          benchDisplay.displayWithMinQuartiles(res, writer)
      case Some((nbIte, duration)) => runPrecisedExperiment(
        ParsedExperimentType(exec, inters, mods, (warmup, rep, opt)), nbIte, duration, writer)
    }
    //Ending of the call : use the gnuplot is existing
    writer.println("=========== Done ===========")
    if (splitedInputLines.length > 1) {
      writer.println("GNUPLOT part is : ")
      writer.println(splitedInputLines(1))
    }
    writer.close
  }
}	
