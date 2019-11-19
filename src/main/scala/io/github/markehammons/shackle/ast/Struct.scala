package io.github.markehammons.shackle.ast

import java.io.File

import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
import io.github.markehammons.shackle.ast.printer.{
  Emittable,
  Line,
  Method,
  MethodCall,
  NameLiteral,
  Object,
  Printable,
  Priv,
  Prot,
  SVAnnotation,
  StringLiteral,
  Trait
}
import io.github.markehammons.shackle.exceptions.AnnotationNotFoundException

import scala.compat.java8.OptionConverters._
import scala.collection.JavaConverters._

//todo
case class Struct(
    nativeLocation: NativeLocation,
    signature: Signature,
    members: List[Member],
    name: ClassName
) extends Printable {
  override def toDotty(): Either[Exception, Seq[Emittable]] = {
    for {
      loc <- nativeLocation.toDotty()
      structAnn <- SVAnnotation(
        "NativeStruct",
        StringLiteral(signature.asDottyString())
      ).toDotty()
      struct <- Trait(
        name,
        Seq(StructType(name)),
        members.flatMap(
          m =>
            m.nativeLocation +: Seq(
              SVAnnotation("NativeGetter", StringLiteral(m.cname)),
              Method(m.name, m.typ),
              SVAnnotation("NativeSetter", StringLiteral(m.cname)),
              Method(
                s"${m.name}_=",
                UnitType,
                Seq(Param("value", m.typ, false))
              ),
              SVAnnotation("NativeAddressof", StringLiteral(m.cname)),
              Method(
                s"${m.name}$$ptr",
                PointerType(m.typ),
                accessModifier = Prot
              )
            )
        ) ++ Seq(
          Object(
            ClassName(Package(Nil), "$pointers"),
            Nil,
            cas = true,
            body = members.map(
              m =>
                Method(
                  m.name,
                  PointerType(m.typ),
                  body = Seq(MethodCall(s"${m.name}$$ptr"))
                )
            )
          )
//          Method(
//            "unary_~",
//            Clazz(ClassName(Package(Nil), "ptrHolder.type")),
//            body = Seq(new Printable {
//              override def toDotty(): Either[Exception, Seq[Emittable]] =
//                Right(Seq(Line("ptrHolder")))
//            })
//          )
        )
      ).toDotty()
    } yield {
      loc ++ structAnn ++ struct
    }
  }
}

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
