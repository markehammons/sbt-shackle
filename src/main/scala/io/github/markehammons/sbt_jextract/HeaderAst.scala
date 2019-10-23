package io.github.markehammons.sbt_jextract

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.TypeDeclaration
import com.github.javaparser.ast.expr.NormalAnnotationExpr

import scala.compat.java8.OptionConverters._
import scala.collection.JavaConverters._


case class HeaderAst(                     name: String,
                                          imports: List[ImportAst],
                                          packageName: String,
                                          path: Option[String],
                resolutionContext: List[String],
                                          globals: List[String],
                libraries: List[String],
                libraryPaths: List[String],
                numericConstants: List[NumericConstantAst],
                nativeFunctions: List[NativeFunctionAst]
               ) {
  override def toString = {
    val annotation = if(path.isDefined || resolutionContext.nonEmpty || libraries.nonEmpty || libraryPaths.nonEmpty) {
      s"""
         |@NativeHeader(
         |  path=${path.map(v => "\"" + v + "\"").getOrElse(???)},
         |  resolutionContext=Array(${resolutionContext.mkString(",")}),
         |  globals=Array(${globals.map(v => "\"" + v + "\"").mkString(",")}),
         |  libraries=Array(${libraries.map(v => "\"" + v + "\"").mkString(",")}),
         |  libraryPaths=Array(${libraryPaths.map(v => "\"" + v + "\"").mkString(",")})
         |)
         |""".stripMargin
    } else ""
    s"""${if(packageName.nonEmpty) s"package $packageName" else ""}
       |
       |${imports.mkString("\n")}
       |$annotation
       |sealed trait $name""".stripMargin
  }
}

object HeaderAst {
  def empty = HeaderAst("", Nil, "", None, Nil, Nil, Nil, Nil, Nil, Nil)

  def fromCompilationUnit(cu: CompilationUnit): HeaderAst = {

    val packageName = cu.getPackageDeclaration.asScala.map(_.getName.asString()).getOrElse(???)
    val imports = cu.getImports.asScala.map(id =>
      ImportAst(ImportAst.pathFromName(id.getName), id.isAsterisk)
    )
    val typ = cu.getPrimaryType.asScala.getOrElse(???)
    val name = typ.getName.asString()
    val headerWAnnotations = typ.getAnnotations.asScala.foldLeft(empty.copy(name = name, imports = imports.toList, packageName = packageName)) {
      case (header, expr: NormalAnnotationExpr) if expr.getName.asString() == "NativeHeader" =>
        expr.getPairs.asScala.foldLeft(header) {
          case (header, mvPair) if mvPair.getName.asString() == "path" && mvPair.getValue.isStringLiteralExpr => header.copy(path = Some(mvPair.getValue.asStringLiteralExpr().getValue()))
          case (header, mvPair) if mvPair.getName.asString() == "globals" && mvPair.getValue.isArrayInitializerExpr => header.copy(globals = mvPair.getValue.asArrayInitializerExpr().getValues.asScala.map(_.asStringLiteralExpr().getValue).toList)
          case (header, mvPair) if mvPair.getName.asString() == "resolutionContext" && mvPair.getValue.isArrayInitializerExpr => header.copy(resolutionContext = mvPair.getValue.asArrayInitializerExpr().getValues.asScala.map(_.asClassExpr().getTypeAsString).toList)
          case (header, mvPair) if mvPair.getName.asString() == "libraries" && mvPair.getValue.isArrayInitializerExpr => header.copy(libraries = mvPair.getValue.asArrayInitializerExpr().getValues.asScala.map(_.asStringLiteralExpr().getValue).toList)
          case (header, mvPair) if mvPair.getName.asString() == "libraryPaths" && mvPair.getValue.isArrayInitializerExpr => header.copy(libraries = mvPair.getValue.asArrayInitializerExpr().getValues.asScala.map(_.asStringLiteralExpr().getValue).toList)
          case (_,mvpair) => throw new Exception(s"unknown mvpair type $mvpair")
        }
      case _ => ???
    }
    headerWAnnotations
  }
}