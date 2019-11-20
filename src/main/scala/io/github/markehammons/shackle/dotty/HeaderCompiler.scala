package io.github.markehammons.shackle.dotty

import java.io.File

import com.github.javaparser.ast.ImportDeclaration
import io.github.markehammons.shackle.ast._
import io.github.markehammons.shackle.ast.{Package => Pkg}
import sbt.io.IO
import io.github.markehammons.shackle.ast.printer.{
  ArrayLiteral,
  ClassLiteral,
  Emittable,
  Indent,
  Line,
  MVAnnotation,
  Method,
  MethodCall,
  NameLiteral,
  Object,
  PackageDeclaration,
  Priv,
  SingleImport,
  StringLiteral,
  Trait,
  VariableDeclaration,
  WildCardImport,
  WildcardExport
}
import sbt._

object HeaderCompiler {
  def dryRun(header: Header, autoSourceDir: File): File = {
    val file = header.name.pkg.path.foldLeft(autoSourceDir) {
      case (p, s) => p / s
    } / s"${header.name.name}.scala"

    file
  }

  def nCompile(header: Header, autoSourceDir: File): File = {
    val file = header.name.pkg.path
      .foldLeft(autoSourceDir)((p, s) => p / s) / s"${header.name.name}.scala"

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
      Nil,
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
        .filter(!_.varargs)
        .map(
          f =>
            Method(
              f.name,
              f.returnType,
              f.parameters,
              Seq(
                MethodCall(
                  s"_theLibrary.${f.name}",
                  f.parameters.map(p => NameLiteral(p.name))
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

    val ast = for {
      pkgDec <- PackageDeclaration(header.name.pkg).toDotty()
      imprt <- WildCardImport(Pkg("java", "foreign", "annotations"))
        .toDotty()
      nhLines <- nh.toDotty
      traitLines <- traitAst.toDotty()
      objectLines <- objectAst.toDotty()
    } yield {
      (pkgDec ++ imprt ++ nhLines ++ traitLines ++ objectLines)
        .map(indenter)
        .mkString("\n")
    }

    ast.fold(throw _, s => { IO.write(file, s); file })
  }

  def compile(header: Header, autoSourceDir: File): File = {

    nCompile(header, autoSourceDir)
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
