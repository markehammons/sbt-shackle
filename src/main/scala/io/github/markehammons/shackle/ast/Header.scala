package io.github.markehammons.shackle.ast

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
import io.github.markehammons.shackle.ast.printer.Printable
import io.github.markehammons.shackle.exceptions.{
  AnnotationKeyNotFound,
  AnnotationNotFoundException,
  NoPrimaryInterfaceInCu,
  PackageDeclarationMissingFromCu
}

import scala.collection.JavaConverters._
import scala.compat.java8.OptionConverters._

case class Header(
    imports: List[Import],
    name: ClassName,
    path: String,
    resolutionContext: List[ClassName],
    libraries: List[String],
    libraryPaths: List[String],
    structs: List[Struct],
    callbacks: List[NativeCallback],
    functions: List[NativeFunction],
    numericConstants: List[NativeNumericConstant],
    nativeStringConstants: List[NativeStringConstant]
)

object Header {
  def fromClassOrInterfaceDelcaration(
      coi: ClassOrInterfaceDeclaration
  ): Either[Exception, Header] = {
    val nativeHeaderAnnotation = coi
      .getAnnotationByName("NativeHeader")
      .asScala
      .toRight(
        new AnnotationNotFoundException(
          s"Could not find @NativeHeader as part of the definition of ${coi.getName.asString()}"
        )
      )
      .map(
        _.asNormalAnnotationExpr().getPairs.asScala
          .map(pair => pair.getName.asString() -> pair.getValue)
          .toMap
      )

    val resolutionContext = nativeHeaderAnnotation.map(
      m =>
        m.get("resolutionContext")
          .toRight(
            new AnnotationKeyNotFound(
              s"resolutionContext was not found as part of @NativeHeader: $m"
            )
          )
          .map(
            _.asArrayInitializerExpr().getValues.asScala.toList
              .map(_.asClassExpr())
              .map(ClassName.fromClassExpr)
          )
          .getOrElse(Nil)
    )

    val libraries = nativeHeaderAnnotation.flatMap(
      m =>
        m.get("libraries")
          .toRight(
            new AnnotationKeyNotFound(
              s"libraries key not found in @NativeHeader: $m"
            )
          )
          .map(
            _.asArrayInitializerExpr().getValues.asScala.toList
              .map(_.asStringLiteralExpr().getValue)
          )
    )

    val libraryPaths = nativeHeaderAnnotation.flatMap(
      m =>
        m.get("libraryPaths")
          .toRight(
            new AnnotationKeyNotFound(
              s"Key libraryPaths not found in @NativeHeader: $m"
            )
          )
          .map(
            _.asArrayInitializerExpr().getValues.asScala.toList
              .map(_.asStringLiteralExpr().getValue)
          )
    )

    val path = nativeHeaderAnnotation.flatMap(
      m =>
        m.get("path")
          .toRight(
            new AnnotationKeyNotFound(
              s"Key path not found in @NativeHeader: $m"
            )
          )
          .map(_.asStringLiteralExpr().getValue)
    )

    val headerName = for {
      name <- coi.getFullyQualifiedName.asScala
        .toRight(new Exception("Could not get header name"))
    } yield ClassName.fromString(
      name
    )

    val structs = Struct.fromHeaderDefinition(coi)

    val callbacks = NativeCallback.fromHeader(coi)

    val functions = NativeFunction.fromHeader(coi)

    val nativeNumericConstant = NativeNumericConstant.fromHeader(coi)

    val nativeStringConstant = NativeStringConstant.fromHeader(coi)

    for {
      rc <- resolutionContext
      l <- libraries
      lps <- libraryPaths
      p <- path
      name <- headerName
      s <- structs
      nnc <- nativeNumericConstant
      nsc <- nativeStringConstant
    } yield Header(
      Nil,
      name,
      p,
      rc,
      l,
      lps,
      s,
      callbacks,
      functions,
      nnc,
      nsc
    )
  }

  def parseFromCompilationUnit(
      cu: CompilationUnit
  ): Either[Exception, Header] = {
    for {
      coi <- cu.getPrimaryType.asScala
        .flatMap(_.toClassOrInterfaceDeclaration.asScala)
        .toRight(
          new NoPrimaryInterfaceInCu(
            "Couldn't find a header interface in the compilation unit"
          )
        )
      pkg <- cu.getPackageDeclaration.asScala
        .map(Package.fromPackageDeclaration)
        .toRight(
          new PackageDeclarationMissingFromCu(
            "No package declaration in compilation unit"
          )
        )
      res <- fromClassOrInterfaceDelcaration(
        coi
      )
    } yield res
  }
}
