package io.github.markehammons.shackle.ast.printer

import io.github.markehammons.shackle.ast.{ClassName, TypeAst}

case class Trait(
    name: ClassName,
    extensions: Seq[TypeAst],
    body: Seq[Printable],
    seal: Boolean = false
) extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = {
    val bodyRepr = body.foldLeft(
      Right(Seq.empty[Emittable]): Either[Exception, Seq[Emittable]]
    ) {
      case (maybeBody, p) => maybeBody.flatMap(s => p.toDotty().map(s ++ _))
    }

    for {
      bodyRep <- bodyRepr
    } yield {
      Line(
        s"${if (seal) "sealed " else ""}trait ${name.name}${if (extensions.nonEmpty)
          " extends "
        else ""}${extensions.map(_.asDottyString).mkString(" with ")}"
      ) +: bodyRep.map(Indent)
    }
  }
}
