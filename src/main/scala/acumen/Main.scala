package acumen

import java.io._

import Errors._
import util.Filters._
import render.ToPython._
import render.Java3D
import Pretty._

import com.sun.j3d.utils.applet.MainFrame

import java.net.{Socket, InetAddress, ServerSocket}
import acumen.Errors.BadProgramOptions

object Main {

  var portNo : Int = 9999
  var serverSocket : ServerSocket = null
  var serverMode : Boolean = false
  var serverBufferedReader : BufferedReader = null
  var serverBufferedWriter : BufferedWriter = null

  def send_recv(s: String) : String = {
    serverBufferedWriter.write(s)
    serverBufferedWriter.newLine()
    serverBufferedWriter.flush()
    serverBufferedReader.readLine()
  }

  def as_ctrace(trace: InterpreterRes) = {
    trace match {case CStoreRes(r) => r; case _ => null}    
  }

  def main(args: Array[String]): Unit = {
    val I = interpreters.reference.Interpreter
    //val I = new interpreters.imperative.parallel.Interpreter(2)

    try {
      /* See if user wants to choose a specific interpreter. */
      val (i: Interpreter, firstNonSemanticsArg: Int) = args(0) match {
        case "--semantics" => args(1) match {
          case "reference" => (interpreters.reference.Interpreter, 2)
          case "parallel-static" => (interpreters.imperative.ParallelInterpreter.static, 2)
          case "parallel-sharing" => (interpreters.imperative.ParallelInterpreter.sharing, 2)
          case "imperative" => (new interpreters.imperative.ImperativeInterpreter, 2)
          case "enclosure" => (interpreters.enclosure.Interpreter, 2)
          case "enclosure-non-localizing" => (interpreters.enclosure.Interpreter.asNonLocalizing, 2)
          case _ => (interpreters.reference.Interpreter, 2) // FIXME: Throw error! -kevina
        }
        case _ => (interpreters.reference.Interpreter, 0)
      }
      /* Read the Acumen source, parse, pre-process and interpret it. */
      lazy val in = new InputStreamReader(new FileInputStream(args(firstNonSemanticsArg)))
      lazy val ast = Parser.run(Parser.prog, in)
      lazy val desugared = Desugarer.run(ast)
      lazy val final_out = desugared // final output after all passes
      lazy val trace = i.run(final_out)
      lazy val ctrace = as_ctrace(trace)
      /* Perform user-selected action. */
      args(firstNonSemanticsArg + 1) match {
        case "compile" => 
          val typeChecker = new TypeCheck(desugared)
          val res = typeChecker.run()
          interpreters.compiler.Interpreter.compile(desugared, typeChecker)
        case "pretty" => println(pprint(ast))
        case "desugar" => println(pprint(desugared))
        case "extract" =>
          val extr = new Extract(desugared)
          println(pprint(extr.res))
        case "typecheck" => 
          val res = new TypeCheck(desugared).run()
          println("\nTYPE CHECK RESULT: " + TypeCheck.errorLevelStr(res) + "\n")
          Pretty.withType = true
          println(pprint(desugared))
        case "3d" => toPython3D(toSummary3D(ctrace))
        case "2d" => toPython2D(toSummary2D(ctrace))
        case "java2d" => new MainFrame(new Java3D(addThirdDimension(ctrace)), 256, 256);
        case "java3d" => new MainFrame(new Java3D(ctrace), 256, 256);
        case "json" => for (st <- ctrace) println(JSon.toJSON(st))
        case "fromJson" =>
          val st = ctrace(0)
          val x = JSon.fromJSON(JSon.toJSON(st).toString)
          println(x)
        case "listen" =>
          println("Model: " + args(firstNonSemanticsArg))
          serverMode = true
          portNo = args(firstNonSemanticsArg + 2).toInt
          serverSocket = new ServerSocket(portNo)
          println("Listening on port " + portNo)
          val socket = serverSocket.accept()
          serverBufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream))
          serverBufferedWriter = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream))
          ctrace.size // Force evaluation of the lazy value
        case "last" =>
          trace.printLast
        case "time" => 
          val forced = final_out
          val startTime = System.currentTimeMillis()
          trace.printLast
          val endTime = System.currentTimeMillis()
          println("Time to run: " + (endTime - startTime)/1000.0)
        case "bench" =>
          val offset = firstNonSemanticsArg + 1 + 1;
          val start: Int = Integer.parseInt(args(offset + 0))
          val stop: Int = Integer.parseInt(args(offset + 1))
          val warmup : Int = if (args.size > offset + 2) Integer.parseInt(args(offset+2)) else 0
          val repeat : Int = if (args.size > offset + 3) Integer.parseInt(args(offset+3)) else 10
          val forced = final_out
          for (nbThreads <- start to stop) {
        	interpreters.imperative.ParallelInterpreter(nbThreads)
            print(nbThreads + " threads: ")
            as_ctrace(i.run(forced)).last
            for (_ <- 0 until warmup) { print("w"); as_ctrace(i.run(forced)).last }
            val startTime = System.currentTimeMillis()
            for (_ <- 0 until repeat) { print("."); as_ctrace(i.run(forced)).last }
            val endTime = System.currentTimeMillis()
            println(endTime - startTime)
          }
        // the first six lines are shared with the "bench" case and would ideally not be repeated
        case "bench-gnuplot" =>
          val offset = firstNonSemanticsArg + 1 + 1;
          val start: Int = Integer.parseInt(args(offset + 0))
          val stop: Int = Integer.parseInt(args(offset + 1))
          val warmup : Int = if (args.size > offset + 2) Integer.parseInt(args(offset+2)) else 0
          val repeat : Int = if (args.size > offset + 3) Integer.parseInt(args(offset+3)) else 10
          val forced = final_out
          var data = Map[Int,Double]()
          for (nbThreads <- start to stop) {
            interpreters.imperative.ParallelInterpreter(nbThreads)
            as_ctrace(i.run(forced)).last
            for (_ <- 0 until warmup) { as_ctrace(i.run(forced)).last }
            val startTime = System.currentTimeMillis()
            for (_ <- 0 until repeat) { as_ctrace(i.run(forced)).last }
            val endTime = System.currentTimeMillis()
            val time = endTime - startTime
            System.err.print(".")
            data += nbThreads -> time 
          }
          // the result would ideally be written to a file, which would allow for 
          // outputting progress information as is done in the "bench" case
          println(Gnuplot.script(data))
        case "bench-enclosures" => 
          BenchEnclosures.run(i, final_out, args, firstNonSemanticsArg + 2)
        case "trace" =>
          trace.print
        case "examples" =>
          var somethingUpdated = false
          Examples.cstoreExamplesAction{(dn, f) =>
            val loc = Examples.expectLoc
            val resFile = Examples.resultFile(loc, dn, f)
            if (resFile.exists) {
              println("skipping " + f + "")
            } else {
              somethingUpdated = true
              Examples.writeExampleResult(loc, dn, f, interpreters.reference.Interpreter.run(_))
              println("PROCESSED " + f)
            }
          }
          if (somethingUpdated) {
            println("Results updated.  Be sure to git add & commit the updated files as appropriate.")
          }
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
