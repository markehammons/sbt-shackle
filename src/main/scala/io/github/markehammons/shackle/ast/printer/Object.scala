package io.github.markehammons.shackle.ast.printer

import io.github.markehammons.shackle.ast.{ClassName, TypeAst}

case class Object(
    name: ClassName,
    extensions: Seq[TypeAst],
    body: Seq[Printable],
    cas: Boolean = false,
    accessModifier: AccessModifier = Pub
) extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = {
    body
      .foldLeft(
        Right(Seq.empty[Emittable]): Either[Exception, Seq[Emittable]]
      )((maybeBody, p) => maybeBody.flatMap(s => p.toDotty().map(s ++ _)))
      .map(
        s =>
          Line(
            s"${accessModifier match {
              case Priv => "private "
              case Prot => "protected "
              case _    => ""
            }}${if (cas) "case " else ""}object ${name.name}${if (extensions.nonEmpty) " extends "
            else ""}${extensions.mkString(" with ")}"
          ) +: s.map(Indent)
      )
  }
}
