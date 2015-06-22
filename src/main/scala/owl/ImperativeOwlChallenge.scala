package owl

/**
 * This generates a little math quiz my daughter was doing at school.
 *
 * a ? b ? c … = x
 *
 * This unpleasant, untyped, imperative solution is predicated on the
 * observation that if you express the math expression as a list
 *    a op b op c … = z
 * uneven elements in the list are values that could be blanked out.
 *
 * The solution generates a list with its answer and randomnly blanks
 * one element out.
 */
object ImperativeOwlChallenge extends App {

  type Question = List[String]

  val n: Int = 60
  val maxValue: Int = 12
  val pSumOrProduct: Double = 0.2
  val pAddOrMinus: Double = 0.5
  val pMultOrDivide: Double = 0.5
  val pFractionOf: Double = 0.1
  val pBlankAnswer: Double = 0.5
  val pTriple: Double = 0.1
  val maxTripleValue: Int = 8
  val blank = "?"
  val nColumns: Int = 3
  val colWidth: Int = 30

  generateTest()

  def generateTest(): Unit = {
    val questions: IndexedSeq[String] = (1 until n + 1).map(i ⇒ example.padTo(colWidth, ' '))
    val columns: Iterator[String] = questions.grouped(nColumns).map(_.mkString)
    println(columns.mkString("\n"))
  }

  def example: String = {
    val question: Question = if (prob(pSumOrProduct)) summation else product
    val blanked = blankSomething(question)
    (blanked.dropRight(1) ::: "=" :: blanked.takeRight(1)).mkString(" ")
  }

  def prob(p: Double): Boolean = math.random < p

  def randomInt(upper: Int = maxValue) = (math.random * upper).toInt + 1

  def blankSomething(question: Question): Question =
    if (question.size > 4 || prob(pBlankAnswer)) question.dropRight(1) :+ blank
    else {
      // remember - simple expressions, every second element is an operator so don't blank those
      val i: Int = ((math.random * question.size).toInt / 2) * 2
      question.take(i) ::: blank :: question.drop(i + 1)
    }

  def summation: Question = if (prob(pAddOrMinus)) add else minus

  def add: Question = {
    val a = randomInt()
    val b = randomInt()
    val List(x, y, z) = List(a, b, a + b).map(_.toString)
    List(x, "+", y, z)
  }

  def minus: Question = {
    val a = randomInt()
    val b = randomInt()
    val (c, d) = if (a > b) (a, b) else (b, a)
    val List(x, y, z) = List(c, d, c - d).map(_.toString)
    List(x, "−", y, z)
  }

  def product: Question = if(prob(pMultOrDivide)) multiply else divide

  def multiply: Question = {
    val (args, result) = multiWork
    intersperse("×", args.map(_.toString)) :+ result.toString
  }

  def divide: Question = if (prob(pFractionOf)) fractionOf else simpleDivision

  def fractionOf: Question = {
    val a = randomInt(50)
    List("½", "of", 2 * a, a).map(_.toString)
  }

  def simpleDivision: Question = {
    val (args, multResult) = multiWork
    val div = multResult :: args.drop(1)
    intersperse("÷", div.map(_.toString)) :+ args(0).toString
  }

  def multiWork: (List[Int], Int) = {
    val (max, nArgs) = if (prob(pTriple)) (maxTripleValue, 3) else (maxValue, 2)
    val args = (0 until nArgs).map(_ ⇒ randomInt(max)).toList
    (args, args.product)
  }

  def intersperse(s: String, l: List[String]): List[String] =
    l match {
      case Nil ⇒ Nil
      case a :: Nil ⇒ l
      case a :: rest ⇒ a :: s :: intersperse(s, rest)
    }
}