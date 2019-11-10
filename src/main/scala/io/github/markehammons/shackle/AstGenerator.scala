package io.github.markehammons.shackle

import java.io.File

import com.github.javaparser.StaticJavaParser
import sbt.Keys.TaskStreams
import sbt.io.syntax.File
import sbt._

object AstGenerator {
  def apply(
      headerFile: File,
      streams: TaskStreams
  ): Either[Exception, ast.Header] = {
    streams.log.debug(s"header: $headerFile")
    val compilationUnit = StaticJavaParser.parse(headerFile)

    val header = ast.Header.parseFromCompilationUnit(compilationUnit)

    header
  }
}
