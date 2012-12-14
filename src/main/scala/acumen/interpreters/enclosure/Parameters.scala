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
