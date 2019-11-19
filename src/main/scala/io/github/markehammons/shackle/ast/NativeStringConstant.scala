package io.github.markehammons.shackle.ast

import com.github.javaparser.ast.body.{
  ClassOrInterfaceDeclaration,
  MethodDeclaration
}
import io.github.markehammons.shackle.ast.printer.{
  Emittable,
  Line,
  Method,
  Printable,
  SVAnnotation,
  StringLiteral
}
import io.github.markehammons.shackle.exceptions.AnnotationNotFoundException

import scala.compat.java8.OptionConverters._
import scala.collection.JavaConverters._

case class NativeStringConstant(
    nativeLocation: NativeLocation,
    name: String,
    value: String
) extends Printable {
  def toDotty(): Either[Exception, Seq[Emittable]] = {
    for {
      nativeRepr <- nativeLocation.toDotty()
      annRepr <- SVAnnotation("NativeStringConstant", StringLiteral(""))
        .toDotty()
      methodRepr <- Method(name, PointerType(ByteType), Nil, Nil).toDotty()
    } yield nativeRepr ++ annRepr ++ methodRepr
  }
}

object NativeStringConstant {
  def fromMethodDeclaration(
      md: MethodDeclaration
  ): Either[Exception, NativeStringConstant] = {
    val value = md
      .getAnnotationByName("NativeStringConstant")
      .asScala
      .toRight(
        new AnnotationNotFoundException(
          s"Could not find @NativeStringConstant defined as part of $md"
        )
      )
      .map(
        _.asSingleMemberAnnotationExpr().getMemberValue
          .asStringLiteralExpr()
          .asString()
      )

    val name = md.getName.asString()

    val nativeLocation = NativeLocation.fromAnnotee(md)

    for {
      nl <- nativeLocation
      v <- value
    } yield NativeStringConstant(nl, name, v)
  }

  def fromHeader(
      coi: ClassOrInterfaceDeclaration
  ): Either[Exception, List[NativeStringConstant]] = {
    coi.getMembers.asScala
      .filter(_.getAnnotationByName("NativeStringConstant").asScala.isDefined)
      .map(_.asMethodDeclaration())
      .foldLeft(
        Right(List.empty[NativeStringConstant]): Either[
          Exception,
          List[NativeStringConstant]
        ]
      ) {
        case (maybeList, md) =>
          maybeList.flatMap(l => fromMethodDeclaration(md).map(_ :: l))
      }
  }
}
