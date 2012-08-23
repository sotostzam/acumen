package acumen.interpreters.enclosure

import org.scalacheck.Gen._
import org.scalacheck.Properties
import org.scalacheck.Prop._

import Generators._
import Interval._
import Types._

object AffineScalarEnclosureTest extends Properties("AffineScalarEnclosure") {

  import TestingContext._

  property("box normalization") =
    forAll(genBox) { box =>
      Box.normalize(box).forall {
        case (_, i) =>
          i greaterThanOrEqualTo Interval(0)
      }
    }

  /* FIXME Negative values for dim are generated! How can this happen? */
  property("box corners consist of box edge endpoints") =
    forAll(choose[Int](1, 10)) { dim =>
      forAll(genDimBox(dim)) { (b: Box) =>
        Box.corners(b).forall { c =>
          c.forall {
            case (name, value) =>
              value == b(name).low || value == b(name).high
          }
        }
      }
    }

  property("there are 2^n corners of a n-dimensional box.") =
    forAll(choose[Int](1, 5)) { dim =>
      forAll(genDimBox(dim)) { box =>
        Box.corners(box).size == scala.math.pow(2, dim)
      }
    }

  property("constant AffineScalarEnclosure has airty 0") =
    forAll(genBox, genInterval) { (box, interval) =>
      AffineScalarEnclosure(box, interval).arity == 0
    }

  property("projection AffineScalarEnclosure has arity 1") =
    forAll(genBox) { box =>
      box.size > 0 ==>
        forAll(choose(0, box.size - 1)) { index =>
          AffineScalarEnclosure(box, box.keys.toList(index)).arity == 1
        }
    }

}