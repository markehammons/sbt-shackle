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
import io.github.markehammons.shackle.exceptions.{
  AnnotationNotFoundException,
  UnrepresentableException
}

import scala.compat.java8.OptionConverters._
import scala.collection.JavaConverters._

case class NativeFunction(
    name: String,
    signature: Signature,
    returnType: TypeAst,
    parameters: List[Param],
    varargs: Boolean
) extends Printable {
  def toDotty(): Either[Exception, Seq[Emittable]] = {
    if (varargs) {
      Left(
        new UnrepresentableException("Dotty cannot express vararg functions")
      )
    } else {
      for {
        annRepr <- SVAnnotation(
          "NativeFunction",
          StringLiteral(signature.string)
        ).toDotty()
        methodRepr <- Method(name, returnType, parameters, Nil).toDotty()
      } yield annRepr ++ methodRepr
    }
  }
}

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
    val params = Param.fromMethodDeclaration(md)

    for {
      sig <- signature
    } yield NativeFunction(
      name,
      sig,
      returnType,
      params,
      params.exists(_.varArgs)
    )
  }
}
