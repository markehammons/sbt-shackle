package io.github.markehammons.shackle.ast.printer

case class ArrayLiteral(literals: Seq[Literal with Renderable])
    extends Literal
    with Renderable {
  override def asDottyString(): String =
    s"Array(${literals.map(_.asDottyString()).mkString(",")})"
}
