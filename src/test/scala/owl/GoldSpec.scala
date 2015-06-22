package owl

import org.scalacheck.{Gen, Arbitrary}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Inspectors, FlatSpec, MustMatchers}
import owl.Expression._

class GoldSpec extends FlatSpec with Gold with GeneratorDrivenPropertyChecks with MustMatchers with Inspectors {

  val genSqrt: Gen[Equation] = Gen.sized { sz =>
    Gen.fromOption(sqrtExpression)
  }

  implicit def sqrtArb: Arbitrary[Equation] = Arbitrary(genSqrt)

  "sqrtExpression" must "have reasonable values" in {
    forAll { (exp: Equation) ⇒
      forAll(allValues(exp)) { i ⇒
        i must be >= 0
        i must be <= 144
      }
    }
  }
}