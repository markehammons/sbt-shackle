package io.github.markehammons.shackle.ast.printer

case class NameLiteral(name: String) extends Literal with Renderable {
  override def asDottyString(): String = name
}
