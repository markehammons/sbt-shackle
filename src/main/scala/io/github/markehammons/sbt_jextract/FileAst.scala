package io.github.markehammons.sbt_jextract

case class FileAst(fileName: String, packageName: String, imports: List[ImportAst], headerAst: HeaderAst)
