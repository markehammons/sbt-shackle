package io.github.markehammons.shackle

import sbt.AutoPlugin
import sbt.io.syntax.File
import sbt._
import sbt.Keys._

import dotty.HeaderCompiler

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

    val useNewGenerator =
      settingKey[Boolean]("whether to use the new generation or not")

    val camelCaseNaming =
      settingKey[Boolean]("headers, structs, and methods use camel-case")
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

      val workDirectory = target.in(compile).value / "jextract" / "javaSources"

      val (extractedJ, cached) = jextract.value

      val sourceDir = (sourceManaged in Compile).value
      val sources = extractedJ
        .map(IO.relativize(workDirectory, _))
        .flatMap(
          _.map(f => sourceDir / s"${f.stripSuffix(".java")}.scala")
            .fold(List.empty[File])(f => List(f))
        )

      val useNgenerator = useNewGenerator.?.value

      if (cached && sources.forall(_.exists())) {
        logger.log.info(s"skipping regenerating of files")
        sources
      } else {
        val asts = extractedJ.map(AstGenerator.apply(_, logger))

        logger.log.debug("completed generation of asts:")

        val renamePhase = asts.map {
          case Right(header) => Renamer.apply(header, Minimal)
          case Left(exception) =>
            sys.error(
              s"${exception.getMessage}\n" + exception.getStackTrace
                .mkString("\n")
            )
        }

        if (useNgenerator.forall(identity)) {
          logger.log.info("using new generator")
          renamePhase.flatMap(HeaderCompiler.nCompile(_, sourceDir))
        } else {
          renamePhase.map(HeaderCompiler.compile(_, sourceDir)).flatMap(Seq(_))
        }
      }
    },
    sourceGenerators in Compile += shackle.taskValue,
    includePaths := Set.empty,
    libraryPaths := Set.empty,
    clangOptions := Set.empty,
    packageMappings := Map.empty
  )

}
