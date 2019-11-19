package io.github.markehammons.shackle.ast

import com.github.javaparser.ast.body.{MethodDeclaration, Parameter}
import io.github.markehammons.shackle.ast.printer.Renderable

import scala.collection.JavaConverters._

case class Param(name: String, typ: TypeAst, varArgs: Boolean)
    extends Renderable {
  def asDottyString: String = {
    s"$name: ${typ.asDottyString}${if (varArgs) "*" else ""}"
  }
}

object Param {
  def fromParameter(parameter: Parameter): Param = {
    Param(
      parameter.getName.asString(),
      TypeAst.fromType(parameter.getType),
      parameter.isVarArgs
    )
  }

  def fromMethodDeclaration(md: MethodDeclaration): List[Param] = {
    md.getParameters.asScala.map(fromParameter).toList
  }
}
