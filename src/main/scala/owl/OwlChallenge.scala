package owl

import java.io._
import sys.process._

/**
 * This code expects pdflatex to be on the path
 */
object OwlChallenge extends Silver with Gold with App {

  val nColumns: Int = 2
  val colWidth: Int = 30
  private val equations: List[Equation] = goldTest.toList

  write(equations, "owl")

  def write(equations: List[Equation], filename: String) = {
    val writer = new PrintWriter(s"$filename.tex", "UTF-8")
    writer.print(LatexRenderer(equations))
    writer.flush()
    writer.close()

    s"pdflatex $filename.tex" #&& s"open $filename.pdf" !
  }
}
