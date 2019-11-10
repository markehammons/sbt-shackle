package io.github.markehammons.shackle.ast

import com.github.javaparser.ast.expr.ClassExpr

case class ClassName(pkg: Package, name: String) {
  def asDottyString: String =
    if (pkg.asDottyString.isBlank) name else s"${pkg.asDottyString}.$name"
}

object ClassName {
  private val pathSplitter = """([a-zA-Z_$][a-zA-Z\d_$]*)\.(.*)""".r
  private val className = """([a-zA-Z_$][a-zA-Z\d_$]*)""".r
  def fromClassExpr(classExpr: ClassExpr): ClassName = {
    fromString(classExpr.getTypeAsString)
  }

  def fromString(string: String): ClassName = {
    val pathList = peelPath(string).reverse
    ClassName(Package(pathList.tail.reverse), pathList.head)
  }

  private def peelPath(string: String): List[String] = {
    string match {
      case pathSplitter(elem, rest) => elem :: peelPath(rest)
      case className(elem)          => List(elem)
      case s                        => throw new Exception(s"couldn't parse $s")
    }
  }
}
