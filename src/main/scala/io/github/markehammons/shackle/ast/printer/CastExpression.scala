package io.github.markehammons.shackle.ast.printer

import io.github.markehammons.shackle.ast.TypeAst

case class CastExpression(lit: Literal, typ: TypeAst) extends Literal {
  override def asDottyString(): String =
    s"${lit.asDottyString()}:${typ.asDottyString}"
}
