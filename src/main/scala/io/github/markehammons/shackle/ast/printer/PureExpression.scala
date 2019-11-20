package io.github.markehammons.shackle.ast.printer

case class PureExpression(literal: Literal) extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = Right(
    Seq(
      Line(s"${literal.asDottyString()}")
    )
  )
}
