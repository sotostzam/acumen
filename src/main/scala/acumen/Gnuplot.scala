package acumen

object Gnuplot {

  // the `data` map takes the number of threads to execution time in milliseconds
  def emitScript(data: Map[Int, Double]) {
    println("# call gnuplot on this file with the --persist option")
    println("set style fill solid")
    println("set boxwidth 0.5")
    println("set yrange [0:*]")
    println("plot \"-\" with boxes")
    for ((threads, time) <- data) {
      println("     " + threads + " " + time)
    }
    println("end")
  }

  def script(data: Map[Int, Double]): String = {
    "# call gnuplot on this file with the --persist option\n" +
      "set style fill solid\n" +
      "set boxwidth 0.5\n" +
      "set yrange [0:*]\n" +
      "plot \"-\" with boxes\n" +
      data.foldLeft("") {
        case (res, (threads, time)) =>
          res + "     " + threads + " " + time + "\n"
      } +
      "end"
  }

}