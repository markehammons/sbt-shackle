package io.github.markehammons.shackle.ast.printer

case class MVAnnotation(
    name: String,
    pairs: (String, Literal with Renderable)*
) extends Printable {
  def toDotty(): Either[Exception, Seq[Emittable]] = {
    Right(
      Seq(
        Line(s"@$name(")
      ) ++ pairs
        .map { case (k, v) => Indent(Line(s"$k=${v.asDottyString()},")) } ++
        Seq(Line(")"))
    )
  }
}
