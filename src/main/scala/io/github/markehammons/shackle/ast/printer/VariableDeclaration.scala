package io.github.markehammons.shackle.ast.printer

import io.github.markehammons.shackle.ast.TypeAst

case class VariableDeclaration(
    nameLiteral: NameLiteral,
    typ: TypeAst,
    body: Seq[Printable],
    mutable: Boolean = false,
    lzy: Boolean = false,
    accessModifier: AccessModifier = Pub
) extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = {
    body
      .foldLeft(Right(Seq.empty): Either[Exception, Seq[Emittable]])(
        (a, b) => a.flatMap(s => b.toDotty().map(r => s ++ r))
      )
      .map(
        em =>
          Line(
            s"${accessModifier match {
              case Priv => "private "
              case Prot => "protected "
              case _    => ""
            }}${if (lzy) "lazy " else ""}${if (mutable) "var" else "val"} ${nameLiteral
              .asDottyString()}: ${typ.asDottyString} = "
          ) +: em.map(Indent)
      )
  }
}
