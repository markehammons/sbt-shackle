package io.github.markehammons.shackle.ast.printer

case class StringLiteral(value: String) extends Literal with Renderable {
  def asDottyString(): String = "\"" + value + "\""
}
