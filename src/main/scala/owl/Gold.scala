package owl

import Expression._

/**
 * 40 questions in 5 minutes
 * Equations have several forms in this test:
 * 1. (a op b) = (c op d), blank anywhere - balanced
 * 2. (a op b) = ? where a or b is large, eg 50 x 4 = 200. Start with small values < 5, multiply them by 10
 * 3. (a op b) op c = d, blank anywhere
 * 4. fraction of a = b where fractions are 1/2, 1/4, 1/5, 2/7 etc
 * 5. Sqrt and squares are permitted - 42 ÷ 7 = sqrt ?, 15 = 10 + sqrt ?, 4^2 - ? = sqrt(25)
 * 6. a x b x c = ?
 * 7. a ÷ b ÷ c = ?
 * 8. a^2 - b^2 - c^2 = ?
 * 9. a x a x a x a = ?
 */
trait Gold {

  def goldTest: Set[Equation] =
    gen(18, balanced) ++
      gen(4, bigMultiply) ++
      gen(3, triple) ++
      gen(3, fractions) ++
      gen(3, tripleProduct) ++
      gen(3, tripleDivideEq) ++
      gen(2, tripleSquaresDifference) ++
      gen(2, quadrupleProduct) ++
      gen(2, sqrtExpression)

  def gen(n: Int, f: ⇒ Option[Equation]): Set[Equation] = {
    def help(s: Set[Equation]): Set[Equation] = if (s.size == n) s else help(s ++ f)

    help(Set.empty)
  }

  def sqrtExpression: Option[Equation] =
    for {
      left ← Option(binaryExpression)
      value ← evaluate(left)
      eq = Equation(left, value)
      replaced ← replaceRandomValue(eq, validSqrt)
    } yield replaced

  def validSqrt(v: Value): Option[Expression] = if (v.i < 12) Some(Sqrt(v.i * v.i)) else None

  // a * b * c * d = ?
  def quadrupleProduct: Option[Equation] = {
    val a = randomInt(3)
    Option(Equation(Multiply(Multiply(Multiply(a, a), a), a), Blank))
  }

  // a^2 - b^2 - c^2 = ?
  def tripleSquaresDifference: Option[Equation] = {
    val a = randomInt(3)
    val b = a + randomInt(3)
    val c = b + randomInt(3)
    Option(
      Equation(
        Minus(
          Minus(Power(c, 2), Power(b, 2)),
          Power(a, 2)),
        Blank))
  }

  // a/b/c = ?
  def tripleDivideEq: Option[Equation] = Option(Equation(tripleDivide, Blank))

  // a * b * c =
  def tripleProduct: Option[Equation] = Option(Equation(Multiply(Multiply(randomInt(8), randomInt(4)), randomInt(4)), Blank))

  def fractions: Option[Equation] = {
    val a1 = randomInt(5)
    val b1 = randomInt(5) + a1

    val gcd = BigInt(a1).gcd(BigInt(b1))

    val a = (a1 / gcd).intValue()
    val b = (b1 / gcd).intValue()

    val e = randomInt(5)

    val c = b * e

    Option(Equation(FractionOf(a, b, c), Blank))
  }

  // Op1(Op2(x,y),z) = a
  def triple: Option[Equation] = {
    // a op b = c
    val outer = binaryExpression
    val leftValue = evaluate(outer.l)

    // d op e = a
    val inner = leftValue.map(binaryExpressionEqualTo(_))

    inner.flatMap { in ⇒
      // substitute (d op e) for a giving ((d op e) op b) = c
      val expression = outer match {
        case Add(a, b) ⇒ Add(in, b)
        case Minus(a, b) ⇒ Minus(in, b)
        case Multiply(a, b) ⇒ Multiply(in, b)
        case Divide(a, b) ⇒ Divide(in, b)
      }

      for {
        result ← evaluate(expression)
        equation = Equation(expression, result)
        if hasReasonableValues(equation)
      } yield equation
    }
  }

  def magMultiplier(maxOrder: Int): Int = math.pow(10d, randomInt(maxOrder).toDouble).toInt

  // multiplying tens and hundreds
  def bigMultiply: Option[Equation] = {
    val a = randomInt(5) * magMultiplier(2)
    val b = randomInt(5) * magMultiplier(2)
    Option(Equation(Multiply(a, b), Blank))
  }

  // generate equations of the form (a op b) = (c op d)
  def balanced: Option[Equation] = {
    val left: Expression = binaryExpression
    val value = evaluate(left)
    val right = value.map(n ⇒ binaryExpressionEqualTo(n))
    for {
      r ← right
      if math.max(maxValueIn(left), maxValueIn(r)) < maxValue * maxValue
    } yield Equation(left, r)
  }

  val binaries = List(Add(_, _), Multiply(_, _), Divide(_, _), Minus(_, _))

  def aBinary = binaries(randomInt(binaries.size) - 1)

  val genBinary: List[() ⇒ BinaryExpression] = List(add, multiply, divide, minus)

  val genBinaryFromValue: List[Value ⇒ BinaryExpression] = List(addEqualToValue, multiplyEqualToValue, divideEqualToValue, minusEqualToValue)

  def add(): Add = Add(randomValue(), randomValue())

  def addEqualToValue(v: Value) = {
    val x = randomInt(v.i)
    val y = v.i - x
    choose(Add(x, y), Add(y, x))
  }

  def multiply(): Multiply = Multiply(randomInt(), randomInt())

  def multiplyEqualToValue(v: Value): Multiply = {
    val fs = factors(v.i)
    val d = oneOf(fs)
    val y = v.i / d
    choose(Multiply(d, y), Multiply(y, d))
  }

  def divide(): Divide = {
    val a = randomInt()
    val b = randomInt()
    val c = a * b

    Divide(c, choose(a, b))
  }

  def divideEqualToValue(v: Value): Divide = {
    val x = randomInt(6)
    val y = x * v.i
    Divide(y, x)
  }

  def minus(): Minus = {
    val a = randomInt()
    val b = randomInt()
    val c = a + b

    Minus(c, choose(a, b))
  }

  def minusEqualToValue(v: Value): Minus = {
    val x = randomInt(10)
    Minus(x + v.i, x)
  }

  def randomBinaryGenerator: Int = (math.random * genBinary.size).toInt

  def randomBinaryFromValueGenerator = (math.random * genBinaryFromValue.size).toInt

  def binaryExpression: BinaryExpression = genBinary(randomBinaryGenerator)()

  def binaryExpressionEqualTo(v: Value): BinaryExpression = genBinaryFromValue(randomBinaryFromValueGenerator)(v)

}
