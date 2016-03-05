package owl

import cats.syntax.cartesian._
import cats.std.option._

sealed trait Expression extends Serializable with Product

case class Value(i: Int) extends Expression
case object Blank extends Expression
case class FractionOf(n: Int, d: Int, e: Expression) extends Expression
case class Sqrt(e: Expression) extends Expression
case class Power(e: Expression, exp: Int) extends Expression

trait BinaryExpression extends Expression {
  def l: Expression
  def r: Expression
}
case class Add(l: Expression, r: Expression) extends Expression with BinaryExpression
case class Multiply(l: Expression, r: Expression) extends Expression with BinaryExpression
case class Minus(l: Expression, r: Expression) extends Expression with BinaryExpression
case class Divide(l: Expression, r: Expression) extends Expression with BinaryExpression

case class Equation(l: Expression, r: Expression)

object Expression {

  implicit def toValue(x: Int): Value = Value(x)

  val blank = "?"
  val maxValue: Int = 12
  val maxReasonableValue: Int = 144
  val maxMinus: Int = 100

  def genProblem(eq: Equation): Option[Equation] = if (hasBlank(eq)) Option(eq) else insertBlank(eq)

  def randomInt(upper: Int = maxValue) = (math.random * upper).toInt + 1

  def randomValue(upper: Int = maxValue): Value = Value(randomInt(upper))

  def simpleMinus: Expression = {
    val (l, r) = (randomInt(maxMinus), randomInt(maxMinus))
    if (l > r) Minus(Value(l), Value(r)) else Minus(Value(r), Value(l))
  }

  def simpleDivide: Expression = {
    val (l, r) = (randomInt(), randomInt())
    Divide(Value(r * l), Value(r))
  }

  def tripleDivide: Expression = {
    val (a, b, c) = (randomInt(4), randomInt(4), randomInt(4))
    val result = Value(a * b * c)
    Divide(Divide(result, Value(b)), Value(c))
  }

  def precedence(expression: Expression): Int =
    expression match {
      case v: Value ⇒ 3
      case Blank ⇒ 3
      case bin: BinaryExpression ⇒
        bin match {
          case a: Add ⇒ 1
          case m: Minus ⇒ 1
          case m: Multiply ⇒ 2
          case d: Divide ⇒ 2
        }
      case h: FractionOf ⇒ 2
      case s: Sqrt ⇒ 3
      case p: Power ⇒ 3
    }

  def evaluate(e: Expression): Option[Int] =
    e match {
      case Value(i) ⇒ Some(i)
      case bin: BinaryExpression ⇒ bin match {
        case Add(l, r) ⇒ evaluateBinary(l, r, _ + _)
        case Minus(l, r) ⇒ evaluateBinary(l, r, _ - _)
        case Multiply(l, r) ⇒ evaluateBinary(l, r, _ * _)
        case Divide(l, r) ⇒
          for {
            divisor ← evaluate(r)
            if divisor > 0
            dividend ← evaluate(l)
          } yield dividend / divisor
      }
      case FractionOf(n, d, v) ⇒ evaluate(v).map(_ * n / d)
      case Power(x, p) ⇒ evaluate(x).map(v ⇒ math.pow(v.toDouble, p.toDouble).toInt)
      case Sqrt(v) ⇒ evaluate(v).map(d ⇒ math.sqrt(d.toDouble).toInt)
      case Blank ⇒ None
    }

  def evaluateBinary(l: Expression, r: Expression, op: (Int, Int) ⇒ Int): Option[Int] =
    for {
      lv ← evaluate(l)
      rv ← evaluate(r)
    } yield op(lv, rv)

  def depth(e: Expression): Int = e match {
    case bin: BinaryExpression ⇒
      bin match {
        case Add(a1, a2) ⇒ binDepth(a1, a2)
        case Multiply(a1, a2) ⇒ binDepth(a1, a2)
        case Minus(a1, a2) ⇒ binDepth(a1, a2)
        case Divide(a1, a2) ⇒ binDepth(a1, a2)
      }
    case Sqrt(v) ⇒ 1 + depth(v)
    case Power(v, p) ⇒ 1 + depth(v)
    case _ ⇒ 1
  }

  def replaceRandomValue(eq: Equation, replacement: Value ⇒ Option[Expression]): Option[Equation] =
    choose(replaceValue(eq.l, replacement).map(c ⇒ eq.copy(l = c)), replaceValue(eq.r, replacement).map(c ⇒ eq.copy(r = c)))

  def insertBlank(eq: Equation): Option[Equation] = replaceRandomValue(eq, _ ⇒ Option(Blank))

  def replaceValue(e: Expression, replacement: Value ⇒ Option[Expression]): Option[Expression] = {
    val values = countValues(e)
    replaceNthValue(e, randomInt(values) - 1, replacement)
  }

  def countValues(e: Expression): Int = {
    def cv(exp: Expression, count: Int): Int =
      exp match {
        case v: Value ⇒ count + 1
        case Blank ⇒ 0
        case bin: BinaryExpression ⇒
          bin match {
            case Add(a, b) ⇒ cv(a, count) + cv(b, count)
            case Minus(a, b) ⇒ cv(a, count) + cv(b, count)
            case Multiply(a, b) ⇒ cv(a, count) + cv(b, count)
            case Divide(a, b) ⇒ cv(a, count) + cv(b, count)
          }
        case FractionOf(_, _, v) ⇒ cv(v, count)
        case Sqrt(v) ⇒ cv(v, count)
        case Power(v, _) ⇒ cv(v, count)
      }
    cv(e, 0)
  }

  def replaceNthValue(e: Expression, n: Int, replacement: Value ⇒ Option[Expression]): Option[Expression] = {
    def bn(exp: Expression, nth: Int): (Option[Expression], Int) = {
      if (nth < 0) (Option(exp), nth)
      else exp match {
        case v: Value if nth == 0 ⇒ (replacement(v), -1)
        case v: Value ⇒ (Option(v), nth - 1)
        case Blank ⇒ (Option(Blank), -1)
        case bin: BinaryExpression ⇒
          bin match {
            case Add(a, b) ⇒
              val (l, c) = bn(a, nth)
              val (r, nc) = bn(b, c)
              ((l |@| r) map ((x, y) ⇒ Add(x, y)), nc)
            case Minus(a, b) ⇒
              val (l, c) = bn(a, nth)
              val (r, nc) = bn(b, c)
              ((l |@| r) map ((x, y) ⇒ Minus(x, y)), nc)
            case Multiply(a, b) ⇒
              val (l, c) = bn(a, nth)
              val (r, nc) = bn(b, c)
              ((l |@| r) map ((x, y) ⇒ Multiply(x, y)), nc)
            case Divide(a, b) ⇒
              val (l, c) = bn(a, nth)
              val (r, nc) = bn(b, c)
              ((l |@| r) map ((x, y) ⇒ Divide(x, y)), nc)
          }
        case FractionOf(_, _, v) ⇒ bn(v, nth)
        case Sqrt(v) ⇒
          val (a, b) = bn(v, nth)
          (a map Sqrt, b)
        case Power(v, _) ⇒ bn(v, nth)
      }
    }
    bn(e, n)._1
  }

  def binDepth(e1: Expression, e2: Expression): Int = 1 + math.max(depth(e1), depth(e2))

  def factors(x: Int): List[Int] =
    if (x == 1) List(1) else (1 to x / 2).filter(x % _ == 0).toList

  def choose[T](a: ⇒ T, b: ⇒ T): T = if (math.random < 0.5) a else b

  def oneOf[T](l: List[T]): T = l((math.random * l.size).toInt)

  def hasBlank(e: Equation): Boolean = hasBlank(e.l) || hasBlank(e.r)

  def hasBlank(e: Expression): Boolean = e match {
    case v:Value ⇒ false
    case Blank ⇒ true
    case bin: BinaryExpression ⇒
      bin match {
        case Add(a, b) ⇒ hasBlank(a) || hasBlank(b)
        case Minus(a, b) ⇒ hasBlank(a) || hasBlank(b)
        case Multiply(a, b) ⇒ hasBlank(a) || hasBlank(b)
        case Divide(a, b) ⇒ hasBlank(a) || hasBlank(b)
      }
    case FractionOf(_, _, v) ⇒ hasBlank(v)
    case Sqrt(v) ⇒ hasBlank(v)
    case Power(v, _) ⇒ hasBlank(v)
  }

  def maxValueIn(e: Expression): Int = {
    def max(exp: Expression, m: Int): Int =
      exp match {
        case v: Value ⇒ math.max(v.i, m)
        case Blank ⇒ m
        case bin: BinaryExpression ⇒
          bin match {
            case Add(a, b) ⇒ max(a, m) + max(b, m)
            case Minus(a, b) ⇒ max(a, m) + max(b, m)
            case Multiply(a, b) ⇒ max(a, m) + max(b, m)
            case Divide(a, b) ⇒ max(a, m) + max(b, m)
          }
        case FractionOf(_, _, v) ⇒ max(v, m)
        case Sqrt(v) ⇒ max(v, m)
        case Power(v, _) ⇒ max(v, m)
      }
    max(e, 0)
  }

  def allValues(e: Equation): Set[Int] = allValues(e.l) ++ allValues(e.r)

  def allValues(e: Expression): Set[Int] =
      e match {
        case v: Value ⇒ Set(v.i)
        case Blank ⇒ Set.empty[Int]
        case bin: BinaryExpression ⇒
          bin match {
            case Add(a, b) ⇒ allValues(a) ++ allValues(b)
            case Minus(a, b) ⇒ allValues(a) ++ allValues(b)
            case Multiply(a, b) ⇒ allValues(a) ++ allValues(b)
            case Divide(a, b) ⇒ allValues(a) ++ allValues(b)
          }
        case FractionOf(_, _, v) ⇒ allValues(v)
        case Sqrt(v) ⇒ allValues(v)
        case Power(v, _) ⇒ allValues(v)
      }

  def hasReasonableValues(e: Equation): Boolean = {
    val all = allValues(e)
    all.min >= 0 && all.max <= maxReasonableValue
  }
}
