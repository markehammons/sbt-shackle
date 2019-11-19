package io.github.markehammons.shackle.ast

import java.io.File

import com.github.javaparser.ast.body.BodyDeclaration
import com.github.javaparser.ast.expr.AnnotationExpr
import io.github.markehammons.shackle.ast.printer.{
  Emittable,
  Indent,
  Line,
  MVAnnotation,
  NumericLiteral,
  Printable,
  StringLiteral
}
import io.github.markehammons.shackle.exceptions.{
  AnnotationKeyNotFound,
  AnnotationNotFoundException
}

import scala.compat.java8.OptionConverters._
import scala.collection.JavaConverters._

case class NativeLocation(file: File, line: Long, column: Long)
    extends Printable {
  def toDotty(): Either[Exception, Seq[Emittable]] = {
    MVAnnotation(
      "NativeLocation",
      "file" -> StringLiteral(file.getCanonicalPath),
      "line" -> NumericLiteral(line.toString),
      "column" -> NumericLiteral(column.toString)
    ).toDotty
  }
}

object NativeLocation {
  def fromAnnotee(t: BodyDeclaration[_]): Either[Exception, NativeLocation] = {
    t.getAnnotationByName("NativeLocation")
      .asScala
      .toRight(
        new AnnotationNotFoundException(
          s"Could not find annotation NativeLocation on $t"
        )
      )
      .map(
        _.asNormalAnnotationExpr().getPairs.asScala
          .map(p => p.getName.asString() -> p.getValue)
          .toMap
      )
      .flatMap(
        m =>
          for {
            file <- m
              .get("file")
              .toRight(
                new AnnotationKeyNotFound(
                  "\"file\" was not defined as part of @NativeLocation of " + t
                    .toString()
                )
              )
              .map(expr => new File(expr.asStringLiteralExpr().getValue))
            line <- m
              .get("line")
              .toRight(
                new AnnotationKeyNotFound(
                  "\"line\" was not defined as part of @NativeLocation of " + t
                    .toString()
                )
              )
              .map(expr => expr.asIntegerLiteralExpr().asInt().toLong)
            column <- m
              .get("column")
              .toRight(
                new AnnotationKeyNotFound(
                  "\"column\" was not defined as part of @NativeLocation of " + t
                    .toString()
                )
              )
              .map(expr => expr.asIntegerLiteralExpr().asInt().toLong)
          } yield NativeLocation(file, line, column)
      )
  }
}
