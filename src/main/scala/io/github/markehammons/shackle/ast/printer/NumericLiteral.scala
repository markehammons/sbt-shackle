package io.github.markehammons.shackle.ast.printer

case class NumericLiteral(t: String) extends Literal with Renderable {
  override def asDottyString(): String = t.toString
}
