package io.github.markehammons.shackle

import java.io.File

import io.github.markehammons.shackle.ast.{
  ByteType,
  Header,
  Member,
  NativeCallback,
  NativeFunction,
  NativeLocation,
  NativeNumericConstant,
  NativeStringConstant,
  PointerType,
  Struct,
  StructType
}
import sbt.io.IO
import sbt._

object HeaderAstCompiler {
  def compile(header: Header, autoSourceDir: File): File = {
    val file = header.name.pkg.path.foldLeft(autoSourceDir) {
      case (p, s) => p / s
    } / s"${header.name.name}.scala"

    println(header.name.name)

    val fileContents =
      s"package ${header.name.pkg.asDottyString}\n" +
        "\n" +
        "import java.foreign.annotations._\n" +
        "\n" +
        "@NativeHeader(\n" +
        "\tpath=\"" + header.path + "\",\n" +
        s"\tlibraries=Array(${header.libraries.map(s => "\"" + s + "\"").mkString(",")}),\n" +
        s"\tlibraryPaths=Array(${header.libraryPaths.map(s => "\"" + s + "\"").mkString(",")}),\n" +
        s"\tresolutionContext=Array(${header.resolutionContext
          .map(c => s"classOf[${c.asDottyString}]")
          .mkString(",")}),\n" +
        ")\n" +
        s"sealed trait ${header.name.name}\n" +
        (if (header.numericConstants.nonEmpty)
           s"${header.numericConstants.map(compile).mkString("\n")}\n"
         else "") +
        (if (header.nativeStringConstants.nonEmpty)
           s"${header.nativeStringConstants.map(compile).mkString("\n")}\n"
         else "") +
        (if (header.functions.nonEmpty)
           s"${header.functions.map(compile).mkString("\n")}\n"
         else "") +
        s"\n\nobject ${header.name.name}\n" +
        s"\timport java.foreign.Libraries\n" +
        s"\timport java.lang.invoke.MethodHandles\n" +
        s"\tprivate val _theLibrary = Libraries.bind(MethodHandles.lookup(),classOf[${header.name.name}])\n" +
        s"${header.structs.map(compile).mkString("\n")}\n" +
        s"${header.callbacks.map(compile).mkString("\n")}\n" +
        s"${header.functions.map(compileStatic(_, header.name.name)).mkString("\n")}\n"

    IO.write(file, fileContents)
    file
  }

  def compile(numericConstant: NativeNumericConstant): String = {
    s"${compile(numericConstant.nativeLocation, 1)}\n" +
      s"\t@NativeNumericConstant(${numericConstant.value})\n" +
      s"\tdef ${numericConstant.name}: ${numericConstant.typ.asDottyString}"
  }

  def compile(stringConstant: NativeStringConstant): String = {
    s"${compile(stringConstant.nativeLocation, 1)}\n" +
      "\t@NativeStringConstant(\"" + stringConstant.value + "\")\n" +
      s"\tdef ${stringConstant.name}: ${PointerType(ByteType).asDottyString}"
  }

  def compile(nativeLocation: NativeLocation, indentationLevel: Int): String = {
    s"${indent(indentationLevel)}@NativeLocation(\n" +
      s"${indent(indentationLevel)}\tfile=" + "\"" + nativeLocation.file.getCanonicalPath + "\",\n" +
      s"${indent(indentationLevel)}\tline=${nativeLocation.line},\n" +
      s"${indent(indentationLevel)}\tcolumn=${nativeLocation.column},\n" +
      s"${indent(indentationLevel)})"
  }

  def compile(nativeFunction: NativeFunction) = {
    "\t@NativeFunction(\"" + nativeFunction.signature + "\")\n" +
      s"\tdef ${nativeFunction.name}(${nativeFunction.parameters.zipWithIndex
        .map { case (t, n) => s"arg$n: ${t.asDottyString}" }
        .mkString(",")}): ${nativeFunction.returnType.asDottyString}"
  }

  def compileStatic(
      nativeFunction: NativeFunction,
      headerName: String
  ): String = {
    s"\tinline def ${nativeFunction.name}(${nativeFunction.parameters.zipWithIndex
      .map { case (t, i) => s"arg$i: ${t.asDottyString}" }
      .mkString(",")}): ${nativeFunction.returnType.asDottyString} = _theLibrary.${nativeFunction.name}(${nativeFunction.parameters.zipWithIndex
      .map { case (_, i) => s"arg$i" }
      .mkString(",")})"
  }

  def compile(struct: Struct): String = {
    s"${compile(struct.nativeLocation, 1)}\n" +
      s"\tsealed trait ${struct.name.name} extends ${StructType(struct.name).asDottyString}\n" +
      s"${struct.members.map(compile).mkString("\n")}\n"
  }

  def compile(member: Member): String = {
    s"${compile(member.nativeLocation, 2)}\n" +
      "\t\t@NativeGetter(\"" + member.cname + "\")\n" +
      s"\t\tdef ${member.name}: ${member.typ.asDottyString}\n" +
      "\t\t@NativeSetter(\"" + member.cname + "\")\n" +
      s"\t\tdef ${member.name}_=(arg: ${member.typ.asDottyString}): Unit\n" +
      "\t\t@NativeAddressof(\"" + member.cname + "\")\n" +
      s"\t\tdef $$${member.name}: ${PointerType(member.typ).asDottyString}"
  }

  def compile(callback: NativeCallback): String = {
    "\t@FunctionalInterface\n" +
      "\t@NativeCallback(\"" + callback.signature.string + "\")\n" +
      s"\tsealed trait ${callback.name.name}\n" +
      s"\t\tdef run(${callback.parameters.zipWithIndex
        .map { case (t, i) => s"arg$i: ${t.asDottyString}" }
        .mkString(",")}): ${callback.returnType.asDottyString}"
  }
  private def indent(level: Int) = "\t".repeat(level)
}
