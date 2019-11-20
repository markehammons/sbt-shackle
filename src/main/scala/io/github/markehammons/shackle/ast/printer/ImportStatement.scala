package io.github.markehammons.shackle.ast.printer

import io.github.markehammons.shackle.ast.ClassName
import io.github.markehammons.shackle.ast.Package
import io.github.markehammons.shackle.exceptions.EmptyListException

sealed trait ImportStatement extends Printable

case class WildCardImport(pkg: Package) extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = Right(
    Seq(Line(s"import ${pkg.asDottyString}._"))
  )
}

case class SingleImport(className: ClassName) extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = Right(
    Seq(Line(s"import ${className.pkg.asDottyString}.${className.name}"))
  )
}

case class MultiImport(pkg: Package, classNames: Seq[String])
    extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] =
    if (classNames.isEmpty) {
      Left(new EmptyListException("Class name list was empty"))
    } else {
      Right(
        Seq(
          Line(
            s"import ${pkg.asDottyString}.{${classNames.mkString(",")}"
          )
        )
      )
    }
}
