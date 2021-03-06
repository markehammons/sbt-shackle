package io.github.markehammons.shackle.dotty

import java.io.File

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.expr.{
  SingleMemberAnnotationExpr,
  StringLiteralExpr
}
import io.github.markehammons.shackle.ast.printer._
import io.github.markehammons.shackle.ast.{Package => Pkg, _}
import sbt._
import sbt.io.IO
import scala.collection.JavaConverters._

object HeaderCompiler {
  def dryRun(cu: CompilationUnit, autoSourceDir: File): Seq[File] = {
    for {
      pkg <- cu.getPackageDeclaration.asScala
        .map(Pkg.fromPackageDeclaration)
      name <- cu.getPrimaryType.asScala.map(_.getName.asString())
      hasVarargs <- cu.getPrimaryType.asScala.map(
        _.getMethods.asScala
          .exists(_.getParameters.asScala.exists(_.isVarArgs))
      )
    } yield {
      val folder = pkg.path.foldLeft(autoSourceDir)((dir, n) => dir / n)
      val scalaFile = folder / s"$name.scala"
      val auxFile = folder / s"$name$$AUX.java"
      if (hasVarargs) {
        Seq(scalaFile, auxFile)
      } else {
        Seq(scalaFile)
      }
    }
  }.getOrElse(Seq.empty)

  def nCompile(header: Header, autoSourceDir: File): Seq[File] = {
    val file = header.name.pkg.path
      .foldLeft(autoSourceDir)((p, s) => p / s) / s"${header.name.name}.scala"
    val auxFile = header.name.pkg.path
      .foldLeft(autoSourceDir)((p, s) => p / s) / s"${header.name.name}$$AUX.java"

    val varargFunctions = header.functions.filter(_.varargs)

    val nh = MVAnnotation(
      "NativeHeader",
      "path" -> StringLiteral(header.path),
      "libraries" -> ArrayLiteral(header.libraries.map(StringLiteral)),
      "libraryPaths" -> ArrayLiteral(header.libraryPaths.map(StringLiteral)),
      "resolutionContext" -> ArrayLiteral(
        header.resolutionContext.map(cn => ClassLiteral(cn.pkg.path, cn.name))
      )
    )

    val traitAst = Trait(
      header.name,
      if (varargFunctions.nonEmpty)
        Seq(Clazz(header.name.copy(name = s"${header.name.name}$$AUX")))
      else Nil,
      header.numericConstants ++ header.nativeStringConstants ++ header.functions
        .filter(!_.varargs),
      true
    )

    val objectAst = Object(
      header.name,
      Nil,
      header.structs ++ header.callbacks ++ Seq(
        SingleImport(
          ClassName(Pkg("java", "foreign"), "Libraries")
        ),
        SingleImport(
          ClassName(Pkg("java", "lang", "invoke"), "MethodHandles")
        ),
        VariableDeclaration(
          NameLiteral("_theLibrary"),
          Clazz(header.name),
          Seq(
            MethodCall(
              "Libraries.bind",
              Seq(
                MethodCall("MethodHandles.lookup"),
                ClassLiteral(header.name.pkg.path, header.name.name)
              )
            )
          ),
          accessModifier = Priv,
          lzy = true
        )
      ) ++ header.functions
        .map(
          f =>
            Method(
              f.name,
              f.returnType,
              f.parameters,
              Seq(
                MethodCall(
                  s"_theLibrary.${f.name}",
                  f.parameters.map(
                    p =>
                      if (p.varArgs)
                        CastExpression(NameLiteral(p.name), VariadicWildcard)
                      else NameLiteral(p.name)
                  )
                )
              ),
              inline = true
            )
        )
    )

    def indenter(em: Emittable): String = em match {
      case Indent(em) => s"\t${indenter(em)}"
      case Line(s)    => s
    }

    if (varargFunctions.nonEmpty) {
      val auxString = {
        val cu = new CompilationUnit()
        cu.addImport("java.foreign.annotations.*")
        val interface = cu.addInterface(s"${header.name.name}$$AUX")

        val varargsMethods = header.functions.filter(_.varargs)
        val methods = varargsMethods.map(m => m -> interface.addMethod(m.name))

        methods.foreach {
          case (m, md) =>
            m.parameters.foreach { p =>
              val param = md.addAndGetParameter(p.typ.javaParserType, p.name)
              if (p.varArgs) {
                param.setVarArgs(true)
              }
              param
            }
            val singleMemberAnnotation = new SingleMemberAnnotationExpr()
            singleMemberAnnotation.setName("NativeFunction")
            singleMemberAnnotation.setMemberValue(
              new StringLiteralExpr().setString(m.signature.string)
            )
            md.setPublic(true)
            md.setType(m.returnType.javaParserType)
            md.addAnnotation(singleMemberAnnotation)
            md.setBody(null)
        }

        cu.setPackageDeclaration(header.name.pkg.asDottyString)
        cu.toString
      }
      IO.write(auxFile, auxString)
    }

    val ast = for {
      pkgDec <- PackageDeclaration(header.name.pkg).toDotty()
      imprt <- WildCardImport(Pkg("java", "foreign", "annotations"))
        .toDotty()
      nhLines <- nh.toDotty()
      traitLines <- traitAst.toDotty()
      objectLines <- objectAst.toDotty()
    } yield {
      (pkgDec ++ imprt ++ nhLines ++ traitLines ++ objectLines)
        .map(indenter)
        .mkString("\n")
    }

    ast.fold(throw _, s => {
      IO.write(file, s)
      if (varargFunctions.nonEmpty) {
        Seq(file, auxFile)
      } else {
        Seq(file)
      }
    })
  }
}
