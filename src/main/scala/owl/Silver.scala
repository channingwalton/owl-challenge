package owl

import Expression._

trait Silver {
  val maxHalf: Int = 25

  def silverProblem: Option[Equation] = {
    val e = silver
    val r = evaluate(e)
    r.map(v ⇒ Equation(e, v))
  }

  def silver: Expression =
    randomInt(60) match {
      case i if i < 40 ⇒ Multiply(Value(randomInt()), Value(randomInt()))
      case i if i < 50 ⇒ simpleDivide
      case i if i < 53 ⇒ FractionOf(1, 2, Value(randomInt(maxHalf) * 2))
      case i if i < 56 ⇒ Multiply(Value(randomInt(3)), Multiply(Value(randomInt(4)), Value(randomInt(4))))
      case _ ⇒ tripleDivide
    }
}
