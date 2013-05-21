package acumen.interpreters.enclosure

/** Parameters for solve-hybrid */
case class Parameters(
  bigDecimalDigits: Int,
  startTime: Double, // simulation start time
  endTime: Double, // simulation end time
  initialPicardPadding: Double, // padding for initial condition in solveVt
  picardImprovements: Int, // number of extra Picard iterations in solveVt
  maxPicardIterations: Int, // maximum number of Picard iterations in solveVt
  maxEventTreeSize: Int, // maximum event tree size in solveVtE, gives termination condition for tree enlargement
  minTimeStep: Double, // minimum time step size
  minSolverStep: Double, // minimum time step size during ODE solving
  minLocalizationStep: Double, // minimum time step size during event localization
  maxTimeStep: Double, // maximum time step size
  minComputationImprovement: Double, // minimum improvement of enclosure
  splittingDegree: Int, // number of pieces to split each initial condition variable
  maxIterations:Int // maximum number of PWL reachable states computation iterations
  ) {
  implicit val rnd = Rounding(bigDecimalDigits)
  val simulationTime = Interval(startTime, endTime)
}
object Parameters {
  // FIXME coercing each integer to a double and back is not ideal...
  val defaults = Map[String, Double](
    "bigDecimalDigits" -> 10,
    "startTime" -> 0,
    "endTime" -> 3,
    "initialPicardPadding" -> 0,
    "picardImprovements" -> 20,
    "maxPicardIterations" -> 200,
    "maxEventTreeSize" -> 30,
    "minTimeStep" -> 0.01,
    "minSolverStep" -> 0.01,
    "minLocalizationStep" -> 0.001,
    "maxTimeStep" -> 3,
    "minComputationImprovement" -> 0.0001,
    "splittingDegree" -> 1,
    "maxIterations" -> 100)
}