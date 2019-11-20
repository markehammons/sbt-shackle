package io.github.markehammons.shackle.ast.printer

import io.github.markehammons.shackle.ast.{Param, TypeAst}

trait MethodT extends Printable {
  def name: String
  def returnType: TypeAst
  def body: Seq[Printable]
  def inline: Boolean
  def accessModifier: AccessModifier
}

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

case class ParenlessMethod(
    name: String,
    returnType: TypeAst,
    inline: Boolean = false,
    accessModifier: AccessModifier = Pub
)(val body: Printable*)
    extends MethodT {
  override def toDotty(): Either[Exception, Seq[Emittable]] = {
    body
      .foldLeft(Right(Seq.empty): Either[Exception, Seq[Emittable]])(
        (maybeSeq, p) =>
          maybeSeq.flatMap(s => p.toDotty().map(v => s ++ v.map(Indent)))
      )
      .map(
        s =>
          Line(s"${accessModifier.asDottyString()}${if (inline) "inline "
          else ""}def $name: ${returnType.asDottyString}${if (s.nonEmpty) " ="
          else ""}") +: s
      )
  }
}
