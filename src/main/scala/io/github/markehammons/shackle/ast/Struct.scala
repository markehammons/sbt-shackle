package io.github.markehammons.shackle.ast

import java.io.File

import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
import io.github.markehammons.shackle.exceptions.AnnotationNotFoundException

import scala.compat.java8.OptionConverters._
import scala.collection.JavaConverters._

//todo
case class Struct(
    nativeLocation: NativeLocation,
    signature: Signature,
    members: List[Member],
    name: ClassName
)

object Struct {
  def fromClassOrInterfaceDeclaration(
      coi: ClassOrInterfaceDeclaration
  ): Either[Exception, Struct] = {
    val annotationMap =
      coi.getAnnotations.asScala.map(aE => aE.getName.asString() -> aE).toMap
    val nativeLocation = {
      NativeLocation.fromAnnotee(coi)
    }
    val signature = {
      coi
        .getAnnotationByName("NativeStruct")
        .asScala
        .toRight(
          AnnotationNotFoundException.fromDeclaration(coi, "NativeStruct")
        )
        .map(
          _.asSingleMemberAnnotationExpr().getMemberValue
            .asLiteralStringValueExpr()
            .getValue
        )
        .map(Signature)
    }

    val members = Member.fromStruct(coi)

    for {
      s <- signature
      ms <- members
      nl <- nativeLocation
      name <- coi.getFullyQualifiedName.asScala
        .toRight(new Exception("No fully qualified name for struct"))
        .map(ClassName.fromString)
    } yield Struct(
      nl,
      s,
      ms,
      name
    )
  }

  def fromHeaderDefinition(
      coi: ClassOrInterfaceDeclaration
  ): Either[Exception, List[Struct]] = {
    coi.getMembers.asScala
      .filter(
        bd =>
          bd.isClassOrInterfaceDeclaration && bd
            .getAnnotationByName("NativeStruct")
            .isPresent
      )
      .map {
        case coi: ClassOrInterfaceDeclaration
            if coi.getAnnotationByName("NativeStruct").isPresent =>
          fromClassOrInterfaceDeclaration(coi)
        case _ => ???
      }
      .toList
      .foldLeft(Right(Nil): Either[Exception, List[Struct]]) {
        case (a, b) => a.flatMap(l => b.map(_ :: l))
      }
      .map(_.reverse)
  }
}
