package io.github.markehammons.shackle.ast.printer

case class MethodCall(name: String, values: Seq[Literal with Renderable] = Nil)
    extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = Right(
    Seq(Line(s"$name(${values.map(_.asDottyString()).mkString(",")})"))
  )
}
