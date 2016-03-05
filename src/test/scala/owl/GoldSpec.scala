package owl

import org.scalatest.{FlatSpec, Inspectors, MustMatchers}
import owl.Expression._

class GoldSpec extends FlatSpec with Gold with MustMatchers with Inspectors {

  val samples: Int = 1000

  "sqrtExpression" must "have reasonable values" in {
    values(sqrtExpression) { i ⇒
      withinReasonableRange(i)
    }
  }

  "balancedExpression" must "have reasonable values" in {
    values(balanced) { i ⇒
      withinReasonableRange(i)
    }
  }

  "tripleExpression" must "have reasonable values" in {
    values(triple) { i ⇒
      withinReasonableRange(i)
    }
  }

  def withinReasonableRange(i: Int): Unit = {
    i must be >= 0
    i must be <= 144
  }

  def values(f: ⇒ Option[Equation])(check: Int ⇒ Unit): Unit =
    forAll(set(f)) { exp ⇒
      val vals= allValues(exp)
      vals.size must be > 0
      forAll(vals)(check) }

  def set(f: ⇒ Option[Equation]): Set[Equation] = gen(samples, f)
}
