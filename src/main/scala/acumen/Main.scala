package acumen

import scala.io._
import java.io.InputStreamReader
import java.io.FileInputStream

import Errors._
import util.Filters._
import interpreters.parallel.Interpreter._
import render.ToPython._
import render.Java3D
import Pretty._

import com.sun.j3d.utils.applet.MainFrame

import javax.swing._

object Main {

  def as_ctrace(trace: InterpreterRes) = {
    trace match {case CStoreRes(r) => r; case _ => null}    
  }

  def main(args: Array[String]): Unit = {
    val I = interpreters.reference.Interpreter
    //val I = new interpreters.parallel.Interpreter(2)

    try {
      /* See if user wants to choose a specific interpreter. */
      val (i, firstNonSemanticsArg) = args(0) match {
        case "--semantics" => args(1) match {
          case "reference" => (interpreters.reference.Interpreter, 2)
          case "parallel" => (new interpreters.parallel.Interpreter(2), 2)
          case "enclosure" => (interpreters.enclosure.Interpreter, 2)
          case "enclosure-non-localizing" => (interpreters.enclosure.InterpreterNonLocalizing, 2)
          case _ => (interpreters.reference.Interpreter, 0)
        }
        case _ => (interpreters.reference.Interpreter, 0)
      }
      /* Read the Acumen source, parse, pre-process and interpret it. */
      val in = new InputStreamReader(
        if (System.in.available != -1) System.in 
        else new FileInputStream(args(firstNonSemanticsArg)))
      lazy val ast = Parser.run(Parser.prog, in)
      lazy val desugared = Desugarer.run(ast)
      lazy val dva_out = DVA.run(desugared)
      lazy val bta_out = BTA.run(dva_out)
      lazy val spec_out = Specializer.run(bta_out)
      lazy val nodiff_out = AD.run(spec_out)
      lazy val trace = i.run(nodiff_out)
      lazy val ctrace = as_ctrace(trace)
      /* Perform user-selected action. */
      args(firstNonSemanticsArg + 1) match {
        case "pretty" => println(pprint(ast))
        case "desugar" => println(pprint(desugared))
        case "3d" => toPython3D(toSummary3D(ctrace))
        case "2d" => toPython2D(toSummary2D(ctrace))
        case "java2d" => new MainFrame(new Java3D(addThirdDimension(ctrace)), 256, 256);
        case "java3d" => new MainFrame(new Java3D(ctrace), 256, 256);
        case "json" => for (st <- ctrace) println(JSon.toJSON(st))
        case "last" =>
          trace.printLast
        case "bench" =>
          val start: Int = Integer.parseInt(args(1))
          val stop: Int = Integer.parseInt(args(2))
          val forced = nodiff_out
          for (nbThreads <- start to stop) {
            print(nbThreads + " threads: ")
            withInterpreter(nbThreads) { PI =>
              as_ctrace(PI.run(forced)).last
              print(".")
              val startTime = System.currentTimeMillis()
              for (_ <- 0 until 10) { print("."); as_ctrace(PI.run(forced)).last }
              val endTime = System.currentTimeMillis()
              println(endTime - startTime)
            }
          }
        case "bench-enclosures" => 
          BenchEnclosures.run(i, nodiff_out, args, firstNonSemanticsArg + 2)
        case "trace" =>
          trace.print
        case what =>
          println(what)
          throw BadProgramOptions(
            List("pretty", "desugar", "3d", "2d", "java2d",
              "java3d", "json", "last", "bench from to", "bench-enclosures ...", "trace"))
      }
    } catch {
      case e: AcumenError =>
        System.err.println(e.getMessage)
        System.exit(1)
      case e => throw e
    }
  }

}
