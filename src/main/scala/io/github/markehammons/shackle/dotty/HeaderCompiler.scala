package io.github.markehammons.shackle.dotty

import java.io.File

import io.github.markehammons.shackle.ast._
import sbt.io.IO
import sbt._

object HeaderCompiler {
  def dryRun(header: Header, autoSourceDir: File): File = {
    val file = header.name.pkg.path.foldLeft(autoSourceDir) {
      case (p, s) => p / s
    } / s"${header.name.name}.scala"

    file
  }

  def compile(header: Header, autoSourceDir: File): File = {
    val file = header.name.pkg.path.foldLeft(autoSourceDir) {
      case (p, s) => p / s
    } / s"${header.name.name}.scala"

    val fileContents =
      s"package ${header.name.pkg.asDottyString}\n" +
        "\n" +
        "import java.foreign.annotations._\n" +
        "import scala.annotation.varargs\n" +
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
           s"${header.functions
             .filter(
               _.parameters
                 .forall(!_.varArgs)
             )
             .map(compile)
             .mkString("\n")}\n"
         else "") +
        s"\n\nobject ${header.name.name}\n" +
        s"\timport java.foreign.Libraries\n" +
        s"\timport java.lang.invoke.MethodHandles\n" +
        s"\tprivate val _theLibrary = Libraries.bind(MethodHandles.lookup(),classOf[${header.name.name}])\n" +
        s"${header.structs.map(compile).mkString("\n")}\n" +
        s"${header.callbacks.map(compile).mkString("\n")}\n" +
        s"${header.functions.filter(_.parameters.forall(!_.varArgs)).map(compileStatic(_, header.name.name)).mkString("\n")}\n"

    IO.write(file, fileContents)
    file
  }

  def compile(numericConstant: NativeNumericConstant): String = {
    s"${compile(numericConstant.nativeLocation, 1)}\n" +
      s"\t@NativeNumericConstant(${numericConstant.value})\n" +
      s"\tdef ${numericConstant.name}: ${numericConstant.typ.asDottyString}\n"
  }

  def compile(stringConstant: NativeStringConstant): String = {
    s"${compile(stringConstant.nativeLocation, 1)}\n" +
      "\t@NativeStringConstant(\"" + stringConstant.value + "\")\n" +
      s"\tdef ${stringConstant.name}: ${PointerType(ByteType).asDottyString}\n"
  }

  def compile(nativeLocation: NativeLocation, indentationLevel: Int): String = {
    s"${indent(indentationLevel)}@NativeLocation(\n" +
      s"${indent(indentationLevel)}\tfile=" + "\"" + nativeLocation.file.getCanonicalPath + "\",\n" +
      s"${indent(indentationLevel)}\tline=${nativeLocation.line},\n" +
      s"${indent(indentationLevel)}\tcolumn=${nativeLocation.column},\n" +
      s"${indent(indentationLevel)})"
  }

  def compile(nativeFunction: NativeFunction) = {
    "\t@NativeFunction(\"" + nativeFunction.signature.string + "\")\n" +
      (if (nativeFunction.parameters.exists(_.varArgs)) "\t@varargs " else "\t") +
      s"def ${nativeFunction.name}(${nativeFunction.parameters
        .map(_.asDottyString)
        .mkString(",")}): ${nativeFunction.returnType.asDottyString}\n"
  }

  def compileStatic(
      nativeFunction: NativeFunction,
      headerName: String
  ): String = {
    (if (nativeFunction.parameters.exists(_.varArgs)) "\t@varargs\n" else "") +
      s"\tinline def ${nativeFunction.name}(${nativeFunction.parameters
        .map(_.asDottyString)
        .mkString(",")}): ${nativeFunction.returnType.asDottyString} = _theLibrary.${nativeFunction.name}(${nativeFunction.parameters
        .map(_.name)
        .mkString(",")})\n"
  }

  def compile(struct: Struct): String = {
    s"${compile(struct.nativeLocation, 1)}\n" +
      "\t@NativeStruct(\"" + struct.signature.string + "\")\n" +
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
      s"\t\tdef $$${member.name}: ${PointerType(member.typ).asDottyString}\n"
  }

  def compile(callback: NativeCallback): String = {
    "\t@FunctionalInterface\n" +
      "\t@NativeCallback(\"" + callback.signature.string + "\")\n" +
      s"\ttrait ${callback.name.name}\n" +
      s"\t\tdef run(${callback.parameters.map(_.asDottyString).mkString(",")}): ${callback.returnType.asDottyString}\n"
  }
  private def indent(level: Int) = "\t".repeat(level)
}
