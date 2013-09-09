package acumen

import org.scalacheck._
import Gen._
import Shrink._
import Arbitrary.arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop._
import acumen.Pretty.pprint
import testutil.ProgGenerator
import testutil.TransformationTestUtil.{
  preservesContinousSemanticsOf
}

object FlattenSimpleTest extends Properties("FlattenSimple") {

  val progGenerator = 
    new ProgGenerator( maxConditionalsPerScope     = 2
                     , maxSimulationTime           = 10.0
                     , maxClassHierarchyDepth      = 2
                     , maxClassHierarchyLayerWidth = 2
                     , minContinuousVarsPerClass   = 2
                     , maxContinuousVarsPerClass   = 4
                     )
  
  import progGenerator.arbProg
  
  property("semantics preserving on continous state (random models)") =
    forAll { (p: Prog) => preservesContinousSemanticsOf(p => InlineInitDeps.proc(FlattenSimple.run(p)), p, None) }

}
