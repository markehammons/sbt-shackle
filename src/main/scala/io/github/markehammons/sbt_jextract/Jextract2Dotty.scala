package io.github.markehammons.sbt_jextract

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.{ImportDeclaration, PackageDeclaration}
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, MethodDeclaration, Parameter}
import com.github.javaparser.ast.expr.NormalAnnotationExpr
import com.github.javaparser.ast.visitor.VoidVisitorAdapter
import sbt.io.syntax.File
import sbt._

import scala.collection.JavaConverters._
import scala.collection.mutable


object Jextract2Dotty {
  def apply(path: File, header: String, outputFile: File): Unit = {
    println(s"header: $header")
    val compilationUnit = StaticJavaParser.parse(path / s"${header}_h.java")
    println(HeaderAst.fromCompilationUnit(compilationUnit))
  }
}


