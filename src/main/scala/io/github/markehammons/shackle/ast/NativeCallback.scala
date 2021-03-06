package io.github.markehammons.shackle.ast

import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
import io.github.markehammons.shackle.ast.printer.{
  Emittable,
  Line,
  Method,
  Printable,
  SVAnnotation,
  StringLiteral,
  Trait
}
import io.github.markehammons.shackle.exceptions.AnnotationNotFoundException

import scala.collection.JavaConverters._
import scala.compat.java8.OptionConverters._

case class NativeCallback(
    name: ClassName,
    signature: Signature,
    returnType: TypeAst,
    parameters: List[Param]
) extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = {
    val t = Trait(
      name,
      Nil,
      Seq(
        Method("run", returnType, parameters, Nil)
      )
    )
    for {
      traitRepr <- t.toDotty()
      annRepr <- SVAnnotation("NativeCallback", StringLiteral(signature.string))
        .toDotty()
    } yield (Line("@FunctionalInterface") +: annRepr) ++ traitRepr
  }
}

object NativeCallback {
  def fromHeader(coi: ClassOrInterfaceDeclaration): List[NativeCallback] = {
    coi.getMembers.asScala
      .filter(
        m =>
          m.isClassOrInterfaceDeclaration && m
            .getAnnotationByName("NativeCallback")
            .isPresent
      )
      .map(_.asClassOrInterfaceDeclaration())
      .map(fromInterface)
      .toList
      .map(_.fold(throw _, identity))
  }

  def fromInterface(
      coi: ClassOrInterfaceDeclaration
  ): Either[AnnotationNotFoundException, NativeCallback] = {
    val signature = coi
      .getAnnotationByName("NativeCallback")
      .asScala
      .toRight(
        AnnotationNotFoundException.fromDeclaration(coi, "NativeCallback")
      )
      .map(
        _.asSingleMemberAnnotationExpr().getMemberValue
          .asStringLiteralExpr()
          .getValue
      )
      .map(Signature)

    val name = ClassName.fromString(coi.getFullyQualifiedName.asScala.get)

    val method = coi.getMethods.asScala.head
    val returnType = TypeAst.fromType(method.getType)

    val params = Param.fromMethodDeclaration(method)

    for {
      s <- signature
    } yield NativeCallback(name, s, returnType, params)
  }
}
