package owl

import java.io._
import sys.process._

/**
 * This code expects pdflatex to be on the path
 */
object OwlChallenge extends Silver with Gold with App {

  val nColumns: Int = 2
  val colWidth: Int = 30

  val writer = new PrintWriter("owl.tex", "UTF-8")
  private val equations: List[Equation] = goldTest.toList

  writer.print(LatexRenderer(equations))
  writer.flush()
  writer.close()

  "pdflatex owl.tex" #&& "open owl.pdf" !
}
