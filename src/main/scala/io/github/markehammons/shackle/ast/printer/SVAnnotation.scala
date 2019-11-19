package io.github.markehammons.shackle.ast.printer

case class SVAnnotation(name: String, value: Literal with Renderable)
    extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = Right(
    Seq(Line(s"@$name(${value.asDottyString()})"))
  )
}
