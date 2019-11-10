package io.github.markehammons.shackle.ast

import com.github.javaparser.ast.PackageDeclaration
import com.github.javaparser.ast.expr.Name
import scala.compat.java8.OptionConverters._

case class Package(path: List[String]) {
  def asDottyString: String = path.mkString(".")
}

object Package {
  def fromPackageDeclaration(pkg: PackageDeclaration): Package = {
    Package(fromName(pkg.getName).reverse)
  }

  private def fromName(name: Name): List[String] = {
    name.getQualifier.asScala
      .map(n => name.getIdentifier :: fromName(n))
      .getOrElse(List(name.getIdentifier))
  }

  def apply(args: String*): Package = {
    Package(args.toList)
  }
}
