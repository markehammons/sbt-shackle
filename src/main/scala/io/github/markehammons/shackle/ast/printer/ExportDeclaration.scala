package io.github.markehammons.shackle.ast.printer

import io.github.markehammons.shackle.exceptions.EmptyListException

trait ExportDeclaration extends Printable {
  def objectName: NameLiteral
}

case class WildcardExport(objectName: NameLiteral) extends ExportDeclaration {
  override def toDotty(): Either[Exception, Seq[Emittable]] =
    Right(
      Seq(
        Line(s"export ${objectName.asDottyString()}._")
      )
    )
}

case class MultiExport(objectName: NameLiteral, memberNames: Seq[NameLiteral])
    extends ExportDeclaration {
  override def toDotty(): Either[Exception, Seq[Emittable]] = {
    if (memberNames.nonEmpty) {
      Right(
        Seq(
          Line(
            s"export ${objectName.asDottyString()}.{${memberNames.map(_.asDottyString()).mkString(",")}}"
          )
        )
      )
    } else {
      Left(
        new EmptyListException(
          "MultiExport requires at least one member export declaration..."
        )
      )
    }
  }
}
