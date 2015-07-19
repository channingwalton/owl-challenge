package owl

import Expression._

object LatexRenderer {

  val times = raw"\times"
  val divide = raw"\div"
  val nl = "\n"

  def \(s: String) = """\""" + s

  def begin(n: String) = \("begin") + s"{$n}" + nl

  def end(n: String) = \("end") + s"{$n}" + nl

  def usepackage(p: String) = \(s"usepackage{$p}") + nl

  def usepackages(ps: String*) = ps.map(usepackage).mkString(nl)

  def documentClass(options: String = "")(clss: String): String =
    (if (options.trim.length > 0) \(s"documentclass[$options]{$clss}") else \(s"documentclass{$clss}")) + nl

  def geometry(g: String*): String = \("geometry") + s"{${g.mkString(",")}}" + nl

  def environment(n: String)(body: String) = begin(n) + body + nl + end(n)

  def document(body: String) = environment("document")(body)

  def deff(cmd: String) = \("def") + \(cmd) + nl

  def section(n: String, numbered: Boolean = true) = (\("section") + (if (numbered) "" else "*") + s"{$n}") + nl

  def tabular(cols: String*)(body: String): String =
    environment("tabular")("{ | " + cols.mkString(" | ") + """ | } \hline""" + nl + body)

  def tableRow(c: Iterable[String]) = c.mkString(" & ")

  def apply(eqs: Iterable[Equation]): String = {
    documentClass("12pt")("article") +
    usepackages("geometry", "mathtools") +
    geometry("a4paper", "margin=1cm") +
    deff("arraystretch{1.5}%") +
    document(
      section("The Golden Owl (5 minutes)", false) +
        environment("flushright")("Score: .................") +
        environment("center")(
          \("Large") +
            tabular("l", "p{4.5cm}", "p{2cm}", "l", "p{4.5cm}", "p{2cm}")(tableBody(eqs))))
  }

  def tableBody(eqs: Iterable[Equation]) = eqs.zipWithIndex.grouped(2).map(rows).mkString(nl)

  def rows(l: Iterable[(Equation, Int)]) = tableRow(l.map( row )) + """ \\ \hline"""

  def row(t: (Equation, Int)): String = {
    val eq = renderEquation(t._1)
    tableRow(Seq((t._2 + 1) + ".", eq, ""))
  }

  def renderEquation(eq: Equation): String = "$" + render(eq.l) + " = " + render(eq.r) + "$"

  def render(e: Expression): String =
    e match {
      case Value(i) ⇒ i.toString
      case Blank ⇒ "\\text{?}"
      case Add(l, r) ⇒ renderBinary("+", e, l, r)
      case Minus(l, r) ⇒ renderBinary("-", e, l, r)
      case Multiply(l, r) ⇒ renderBinary(times, e, l, r)
      case Divide(l, r) ⇒ renderBinary(divide, e, l, r)
      case FractionOf(n, d, exp) ⇒
        val r = render(exp)
        val bracketed = bracket(r, precedence(e) > precedence(exp))
        fraction(n, d) + s"""\\text{ of }$bracketed"""
      case Sqrt(v) ⇒ \(s"sqrt{${render(v)}}")
      case Power(v, p) ⇒
        val r = render(v)
        if (depth(v) > 1) s"($r)^$p" else s"$r^$p"
    }

  def fraction(n: Int, d: Int): String = s"\\frac{$n}{$d}"

  def bracket(s:String, brackets: Boolean): String = if (brackets) s"($s)" else s

   def renderBinary(op: String, parent: Expression, l: Expression, r: Expression): String = {
    val sl = render(l)
    val sr = render(r)

    s"${bracket(sl, precedence(parent) > precedence(l))} $op ${bracket(sr, precedence(parent) > precedence(r))}"
  }
}
