package io.github.markehammons.shackle.java_compiler

import java.io.File

import com.github.javaparser
import com.github.javaparser.ast.CompilationUnit
import io.github.markehammons.shackle.ast.Header

object BaseHeaderCompiler {
  def apply(header: Header, autoSourceDir: File) = {
    val compilationUnit = new CompilationUnit()

    compilationUnit.setPackageDeclaration(header.name.pkg.path.mkString("."))
    val interfaceName = s"${header.name.name}_varargs"
    compilationUnit.addInterface(s"${header.name.name}_varargs")
  }
}
