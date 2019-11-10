package io.github.markehammons.shackle.ast

import com.github.javaparser.ast.body.{
  ClassOrInterfaceDeclaration,
  MethodDeclaration
}
import com.github.javaparser.ast.expr.{
  Expression,
  IntegerLiteralExpr,
  LiteralExpr,
  LongLiteralExpr
}
import io.github.markehammons.shackle.exceptions.AnnotationKeyNotFound

import scala.compat.java8.OptionConverters._
import scala.collection.JavaConverters._

case class NativeNumericConstant(
    nativeLocation: NativeLocation,
    typ: TypeAst,
    name: String,
    value: String
)

object NativeNumericConstant {
  def fromMethodDeclaration(
      md: MethodDeclaration
  ): Either[Exception, NativeNumericConstant] = {
    val nativeLocation = NativeLocation.fromAnnotee(md)
    val typ = TypeAst.fromType(md.getType)
    val name = md.getName.asString()
    val value = md
      .getAnnotationByName("NativeNumericConstant")
      .asScala
      .toRight(
        new AnnotationKeyNotFound(
          "Could not find annotation \"NativeNumericConstant\" on " + md
            .toString()
        )
      )
      .flatMap(
        _.asSingleMemberAnnotationExpr().getMemberValue match {
          case l: LongLiteralExpr    => Right(l.getValue)
          case i: IntegerLiteralExpr => Right(i.getValue)
          case expr: LiteralExpr     => Right(expr.toString)
          case e: Expression =>
            Right(e.toString) //todo: what is the expression of -121141L?
          case e => Left(new Exception(s"unknown expression $e"))
        }
      )

    for {
      nativeLoc <- nativeLocation
      v <- value
    } yield NativeNumericConstant(nativeLoc, typ, name, v)
  }

  def fromHeader(
      coi: ClassOrInterfaceDeclaration
  ): Either[Exception, List[NativeNumericConstant]] = {
    coi.getMembers.asScala.toList
      .filter(
        bd =>
          bd.getAnnotationByName("NativeNumericConstant")
            .asScala
            .isDefined && bd.isMethodDeclaration
      )
      .map(bd => fromMethodDeclaration(bd.asMethodDeclaration()))
      .foldLeft(
        Right(List.empty[NativeNumericConstant]): Either[Exception, List[
          NativeNumericConstant
        ]]
      ) {
        case (maybeList, maybeConstant) =>
          maybeList.flatMap(l => maybeConstant.map(c => c :: l))
      }
      .map(_.reverse)
  }
}
