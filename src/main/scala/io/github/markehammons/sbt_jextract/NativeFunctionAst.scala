package io.github.markehammons.sbt_jextract

case class NativeFunctionAst(locationAst: Option[LocationAst], name: String, returnType: TypeAst, parameters: List[ParameterAst]) {
  override def toString: String =
    locationAst.map(l => s"$l\n").getOrElse("") +
    s"""
         |def $name(${parameters.mkString(",")}): $returnType\n
         |""".stripMargin
}
