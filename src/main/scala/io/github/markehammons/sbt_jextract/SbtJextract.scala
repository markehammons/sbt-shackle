package io.github.markehammons.sbt_jextract

import java.io.{ByteArrayOutputStream, PrintWriter}
import java.util.spi.ToolProvider

import sbt.AutoPlugin
import sbt.io.syntax.File
import sbt._
import sbt.Keys._



object SbtJextract extends AutoPlugin {
  def runTool(name: String, arguments: String*): Either[String,(String,String)] = {
    val maybeTool: Option[ToolProvider] = {
      val _tool = ToolProvider.findFirst(name)
      if(_tool.isPresent) {
        Some(_tool.get())
      } else {
        None
      }
    }

    val result = for(tool <- maybeTool) yield {
      val stdOut = new ByteArrayOutputStream()
      val errOut = new ByteArrayOutputStream()
      val stdWriter = new PrintWriter(stdOut)
      val errWriter = new PrintWriter(errOut)
      val code = tool.run(stdWriter, errWriter, arguments: _*)
      stdWriter.close()
      errWriter.close()
      (code, new String(stdOut.toByteArray), new String(errOut.toByteArray))
    }

    result
      .toRight(s"Could not find tool $name in your java development environment")
      .flatMap{ case (code,ret,err) =>
        if(code != 0) {
          Left(s"failure with code $code: ${ret + err}")
        } else {
          Right(ret -> err)
        }
      }
  }

  val regex = """(.*)_(h|lib)\.java""".r

  def recursiveFileScan(file: File): Unit = {
    val headers = file.listFiles().filter(_.isFile).map(_.getName).map {
      case regex(headername,_) => headername
      case _ => ???
    }
    val folders = file.listFiles().filter(_.isDirectory)

    headers.foreach(h => Jextract2Dotty.apply(file, h, file))
    folders.foreach(recursiveFileScan)
  }


  object autoImport {
    val includeDirectory = settingKey[File]("location of your system's default include directory")
    val headers = settingKey[Set[File]]("header files to pass to jextract")
//    val xdgShellProtocolLocation = settingKey[File]("location of xdg-shell-unstable-v6.xml on your system")
    val includePaths = settingKey[Set[File]]("paths to include directories that jextract will need")
    val libraryPaths = settingKey[Set[File]]("paths to libraries that jextract will link to")
    val clangOptions = settingKey[Set[String]]("settings for jextract to pass to clang")
    val library = settingKey[String]("library to link to")
    val libraryDirectory = settingKey[File]("location of .so files on your system")
    val libraryPackage = settingKey[String]("the package the library bindings will be put in")
    val outputLibraryName = settingKey[String]("The output jar name")
    val packageMappings = settingKey[Map[File, String]]("A mapping of include folders to packages")

    val jextract = taskKey[Unit]("generates a java binding for the configured library")
    val scalaExtract = taskKey[Unit]("generates a scala binding for the configured library")

//    lazy val xdgProtocolGen = taskKey[File]("generates the xdg-shell-protocol.h header")
  }

  import autoImport._

  override lazy val projectSettings = Seq(
    jextract := {
      val logger = streams.value


      val outputFile = unmanagedBase.value / s"${outputLibraryName.value}"

      outputFile.getParentFile().mkdir()

      if(outputFile.exists()) {
        logger.log.warn("deleting already generated binding")
        IO.delete(outputFile)
      }

      val IPaths = includePaths.value.toSeq.flatMap(f => Seq("-I", f.getCanonicalPath))
      val LPaths = libraryPaths.value.toSeq.flatMap(f => Seq("-L", f.getCanonicalPath))
      val mappings = packageMappings.value.toSeq.flatMap{ case (loc, pack) => Seq("--package-map", s"${loc.getCanonicalPath}=$pack")}
      val clangOpts = clangOptions.value.toSeq.flatMap(opt => Seq("-C", opt))
      val headerList = headers.value.toSeq.map(_.getCanonicalPath)
      val command = headerList ++ mappings ++ IPaths ++ clangOpts ++ LPaths ++
        Seq("--record-library-path", "-l", library.value, "-t", libraryPackage.value, "-o", outputFile.getCanonicalPath)

      logger.log.info(s"issuing command jextract ${command.mkString(" ")}")

      runTool("jextract", command: _*).fold(sys.error, { case (out,err) =>
        logger.log.warn(err)
        logger.log.info(out)
      })
    },
    scalaExtract := {
      val logger = streams.value


      val outputFile = target.value / "scala-extract" / s"${outputLibraryName.value}"

      outputFile.mkdir()

//      if(outputFile.exists()) {
//        logger.log.warn("deleting already generated binding")
//        IO.delete(outputFile)
//      }

      val IPaths = includePaths.value.toSeq.flatMap(f => Seq("-I", f.getCanonicalPath))
      val LPaths = libraryPaths.value.toSeq.flatMap(f => Seq("-L", f.getCanonicalPath))
      val mappings = packageMappings.value.toSeq.flatMap{ case (loc, pack) => Seq("--package-map", s"${loc.getCanonicalPath}=$pack")}
      val clangOpts = clangOptions.value.toSeq.flatMap(opt => Seq("-C", opt))
      val headerList = headers.value.toSeq.map(_.getCanonicalPath)
      val command = headerList ++ mappings ++ IPaths ++ clangOpts ++ LPaths ++
        Seq("--record-library-path", "-l", library.value, "-t", libraryPackage.value, "--src-dump-dir", outputFile.getCanonicalPath)

      logger.log.info(s"issuing command jextract ${command.mkString(" ")}")

      val res = runTool("jextract", command: _*).fold(sys.error, { case (out,err) =>
        logger.log.warn(err)
        logger.log.info(out)
      })

      recursiveFileScan(outputFile)
    },
    includePaths := Set.empty,
    libraryPaths := Set.empty,
    clangOptions := Set.empty,
    packageMappings := Map.empty
  )

}
