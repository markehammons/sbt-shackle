package io.github.markehammons.shackle.ast.printer

case class MethodCall(name: String, values: Seq[Literal with Renderable] = Nil)
    extends Printable
    with Literal {
  override def toDotty(): Either[Exception, Seq[Emittable]] = Right(
    Seq(Line(s"$name(${values.map(_.asDottyString()).mkString(",")})"))
  )

  override def asDottyString(): String =
    s"$name(${values.map(_.asDottyString()).mkString(",")})"
}
