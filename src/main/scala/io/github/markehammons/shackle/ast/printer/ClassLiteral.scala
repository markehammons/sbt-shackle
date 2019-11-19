package io.github.markehammons.shackle.ast.printer

case class ClassLiteral(path: List[String], name: String)
    extends Literal
    with Renderable {
  override def asDottyString(): String = {
    if (path.isEmpty)
      s"classOf[$name]"
    else
      s"classOf[${path.mkString(".")}.$name]"
  }
}
