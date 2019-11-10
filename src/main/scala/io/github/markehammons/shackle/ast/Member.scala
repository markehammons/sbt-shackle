package io.github.markehammons.shackle.ast

import com.github.javaparser.ast.body.{
  ClassOrInterfaceDeclaration,
  MethodDeclaration
}
import io.github.markehammons.shackle.exceptions.AnnotationNotFoundException

import scala.compat.java8.OptionConverters._
import scala.collection.JavaConverters._

case class Member(
    nativeLocation: NativeLocation,
    name: String,
    typ: TypeAst,
    cname: String
)

object Member {
  def fromMethodDeclaration(
      methodDeclaration: MethodDeclaration
  ): Either[Exception, Member] = {
    val name = methodDeclaration
      .getAnnotationByName("NativeGetter")
      .asScala
      .toRight(
        AnnotationNotFoundException
          .fromDeclaration(methodDeclaration, "NativeGetter")
      )
      .map(
        _.asSingleMemberAnnotationExpr().getMemberValue
          .asStringLiteralExpr()
          .getValue
      )

    val nativeLocation = NativeLocation.fromAnnotee(methodDeclaration)
    val typ = TypeAst.fromType(methodDeclaration.getType)
    for {
      n <- name
      loc <- nativeLocation
    } yield Member(loc, n, typ, n)
  }

//  def fromStruct(coi: ClassOrInterfaceDeclaration): List[Member] = {
//    val members = coi.getMembers.asScala
//      .filter(_.getAnnotationByName("NativeGetter").asScala.isDefined)
//      .map(_.asMethodDeclaration())
//
//    members.map(fromMethodDeclaration).toList
//  }

  def fromStruct(
      coi: ClassOrInterfaceDeclaration
  ): Either[Exception, List[Member]] = {
    val members = coi.getMembers.asScala
      .filter(_.getAnnotationByName("NativeGetter").asScala.isDefined)
      .map(_.asMethodDeclaration())

    members
      .map(fromMethodDeclaration)
      .toList
      .foldLeft(
        Right(Nil): Either[Exception, List[Member]]
      ) {
        case (a, b) => a.flatMap(l => b.map(_ :: l))
      }
      .map(_.reverse)
  }
}
