package io.github.markehammons.shackle.ast.printer

import io.github.markehammons.shackle.ast.Package

case class PackageDeclaration(pkg: Package) extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = Right(
    Seq(
      Line(s"package ${pkg.asDottyString}")
    )
  )
}
