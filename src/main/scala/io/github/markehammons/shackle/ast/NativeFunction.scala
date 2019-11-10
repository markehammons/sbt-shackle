package io.github.markehammons.shackle.ast

import com.github.javaparser.ast.body.{
  ClassOrInterfaceDeclaration,
  MethodDeclaration
}
import io.github.markehammons.shackle.exceptions.AnnotationNotFoundException

import scala.compat.java8.OptionConverters._
import scala.collection.JavaConverters._

case class NativeFunction(
    name: String,
    signature: Signature,
    returnType: TypeAst,
    parameters: List[TypeAst]
)

object NativeFunction {
  def fromHeader(coi: ClassOrInterfaceDeclaration): List[NativeFunction] = {
    coi.getMembers.asScala
      .filter(
        bd =>
          bd.isMethodDeclaration && bd
            .getAnnotationByName("NativeFunction")
            .isPresent
      )
      .map(_.asMethodDeclaration())
      .map(md => fromMethodDeclaration(md).fold(throw _, identity))
      .toList
  }

  def fromMethodDeclaration(
      md: MethodDeclaration
  ): Either[AnnotationNotFoundException, NativeFunction] = {
    val name = md.getName.asString()
    val signature = md
      .getAnnotationByName("NativeFunction")
      .asScala
      .toRight(
        AnnotationNotFoundException.fromDeclaration(md, "NativeFunction")
      )
      .map(
        _.asSingleMemberAnnotationExpr().getMemberValue
          .asStringLiteralExpr()
          .asString()
      )
      .map(Signature)
    val returnType = TypeAst.fromType(md.getType)
    val params =
      md.getParameters.asScala.map(_.getType).toList.map(TypeAst.fromType)

    for {
      sig <- signature
    } yield NativeFunction(name, sig, returnType, params)
  }
}
