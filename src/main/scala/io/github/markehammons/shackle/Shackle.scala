package io.github.markehammons.shackle

import com.github.javaparser.StaticJavaParser
import sbt.AutoPlugin
import sbt.io.syntax.File
import sbt._
import sbt.Keys._
import dotty.HeaderCompiler
import ast.Header

object Shackle extends AutoPlugin {
  object autoImport {
    val headers = settingKey[Set[File]]("header files to pass to jextract")
    val includePaths = settingKey[Set[File]](
      "paths to include directories that jextract will need"
    )
    val libraryPaths =
      settingKey[Set[File]]("paths to libraries that jextract will link to")
    val clangOptions =
      settingKey[Set[String]]("settings for jextract to pass to clang")
    val library = settingKey[String]("library to link to")
    val libraryPackage =
      settingKey[String]("the package the library bindings will be put in")
    val outputLibraryName = settingKey[String]("The output jar name")
    val packageMappings =
      settingKey[Map[File, String]]("A mapping of include folders to packages")
    val jextract =
      taskKey[(Seq[File], Boolean)](
        "generates java binding code for the targeted library"
      )
    val shackle =
      taskKey[Seq[File]]("generates a scala binding for the configured library")
  }

  import autoImport._

  override lazy val projectSettings = Seq(
    jextract := {
      implicit val logger = streams.value

      val workDirectory = target.in(compile).value / "jextract"

      JavaExtract(
        workDirectory,
        includePaths.value.toSeq,
        libraryPaths.value.toSeq,
        packageMappings.value,
        clangOptions.value,
        headers.value,
        library.value,
        libraryPackage.value
      )
    },
    shackle := {
      val logger = streams.value

      val workDirectory = target.in(compile).value / "shackle"

      val filesCache = workDirectory / "files"

      val (extractedJ, cached) = jextract.value

      val sourceDir = (sourceManaged in Compile).value

      val outputs =
        if (filesCache.exists()) IO.readLines(filesCache).map(file)
        else Seq.empty

      if (cached && filesCache.exists() && outputs
            .forall(_.exists())) {
        logger.log.info(s"skipping regenerating of files")
        outputs
      } else {
        val cus = extractedJ.map(StaticJavaParser.parse)

        val asts = cus.map(Header.parseFromCompilationUnit)

        logger.log.debug("completed generation of asts:")

        val renamePhase = asts.map {
          case Right(header) => Renamer.apply(header)
          case Left(exception) =>
            sys.error(
              s"${exception.getMessage}\n" + exception.getStackTrace
                .mkString("\n")
            )
        }

        val filesOut =
          renamePhase.flatMap(HeaderCompiler.nCompile(_, sourceDir))

        IO.writeLines(
          filesCache,
          filesOut.map(_.getCanonicalPath)
        )

        filesOut
      }
    },
    sourceGenerators in Compile += shackle.taskValue,
    includePaths := Set.empty,
    libraryPaths := Set.empty,
    clangOptions := Set.empty,
    packageMappings := Map.empty
  )

}
