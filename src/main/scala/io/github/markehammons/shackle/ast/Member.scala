package io.github.markehammons.shackle.ast

import com.github.javaparser.ast.body.{
  ClassOrInterfaceDeclaration,
  MethodDeclaration
}
import io.github.markehammons.shackle.ast.printer.{
  Emittable,
  Method,
  MethodCall,
  NameLiteral,
  Object,
  Printable,
  SVAnnotation,
  StringLiteral
}
import io.github.markehammons.shackle.exceptions.AnnotationNotFoundException

import scala.compat.java8.OptionConverters._
import scala.collection.JavaConverters._

case class Member(
    nativeLocation: NativeLocation,
    name: String,
    typ: TypeAst,
    cname: String
) {

//  override def toDotty(): Either[Exception, Seq[Emittable]] = {
//    for {
//      nLoc <- nativeLocation.toDotty()
//      getterAnn <- SVAnnotation("NativeGetter", StringLiteral(cname)).toDotty()
//      setterAnn <- SVAnnotation("NativeSetter", StringLiteral(cname)).toDotty()
//      pointerAnn <- SVAnnotation("NativeAddressof", StringLiteral(cname))
//        .toDotty()
//      getterM <- Method(name, priv = true).toDotty()
//      setterM <- Method(
//        s"${name}$$set",
//        UnitType,
//        Seq(Param(name, typ, false)),
//        priv = true
//      ).toDotty()
//      pointerM <- Method(s"${name}$$ptr", PointerType(typ), priv = true)
//        .toDotty()
//      obj <- Object(
//        ClassName(Package(Nil), name),
//        Nil,
//        Seq(
//          Method("apply", typ, Nil, Seq(MethodCall(s"${name}$$get"))),
//          Method(
//            "_=",
//            UnitType,
//            Seq(Param("value", typ, false)),
//            Seq(MethodCall(s"${name}$$set", Seq(NameLiteral("value"))))
//          ),
//          Method()
//        )
//      )
//    } yield {
//      nLoc ++ getterAnn ++ getterM ++ setterAnn ++ setterM ++ pointerAnn ++ pointerM
//    }
//  }
}

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
