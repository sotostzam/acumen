package benchTool

/**
 * Trait for any way to run the measurements
 */
trait Measurer {
  type timeT = bench.timeT
  type LtimeT = bench.LtimeT
  type LLtimeT = bench.LLtimeT
  type partialResultT = bench.partialResultT
  type ResultT = bench.ResultT

  def run(mods: List[String], inters: List[String], timeToSpendinSec: timeT, 
      nbIte: Int): ResultT
}	
