package io.github.markehammons.shackle.ast.printer

import io.github.markehammons.shackle.ast.{Param, TypeAst}

case class Method(
    name: String,
    returnType: TypeAst,
    parameters: Seq[Param] = Nil,
    body: Seq[Printable] = Nil,
    inline: Boolean = false,
    accessModifier: AccessModifier = Pub
) extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = {
    val bodyRepr = body.foldLeft(
      Right(Seq.empty[Emittable]): Either[Exception, Seq[Emittable]]
    ) {
      case (maybeSeq, p) =>
        maybeSeq.flatMap(s => p.toDotty().map(v => s ++ v.map(Indent)))
    }

    for {
      bodyRep <- bodyRepr
    } yield {
      Line(
        s"${accessModifier match {
          case Priv => "private "
          case Prot => "protected "
          case _    => ""
        }}${if (inline) "inline " else ""}def $name(${parameters
          .map(_.asDottyString)
          .mkString(",")}): ${returnType.asDottyString}${if (bodyRep.nonEmpty) " ="
        else ""}"
      ) +: bodyRep
    }
  }
}
