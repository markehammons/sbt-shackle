package io.github.markehammons.shackle.ast

import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.expr.Name
import scala.compat.java8.OptionConverters._

case class Import(path: List[String], wildcard: Boolean)

object Import {
  def fromImportDeclaration(id: ImportDeclaration): Import = {
    Import(buildPath(id.getName), id.isAsterisk)
  }

  private def buildPath(name: Name): List[String] = {
    name.getQualifier.asScala
      .map(n => name.getIdentifier :: buildPath(n))
      .getOrElse(List(name.getIdentifier))
      .reverse
  }
}
