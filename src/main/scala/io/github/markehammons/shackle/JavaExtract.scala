package io.github.markehammons.shackle

import java.security.MessageDigest
import java.util.Base64

import sbt.Keys.TaskStreams
import sbt.io.syntax.File
import sbt._

object JavaExtract {
  private val regex = """(.*)_(h|lib)\.java""".r

  private def recursiveProcessing[T](
      file: File,
      streams: TaskStreams
  )(fn: (File, String, TaskStreams) => T): Seq[T] = {
    val headers = file.listFiles().filter(_.isFile).map(_.getName).map {
      case regex(headername, _) => headername
      case s                    => ???
    }

    val folders = file.listFiles().filter(_.isDirectory)
    val fs = headers.map(h => fn(file, h, streams))
    folders.flatMap(recursiveProcessing(_, streams)(fn)) ++ fs
  }

  def apply(
      workDirectory: File,
      includePaths: Seq[File],
      libraryPaths: Seq[File],
      packageMappings: Map[File, String],
      clangOptions: Set[String],
      headerIncludes: Set[File],
      library: String,
      libraryPackage: String
  )(implicit logger: TaskStreams): (Seq[File], Boolean) = {
    val cacheSum = workDirectory / "cache.md5"
    val libSum = workDirectory / "lib.md5"
    val commandSum = workDirectory / "command.md5"

    val javaDumpLocation = workDirectory / "javaSources"

    val md = MessageDigest.getInstance("MD5")
    val base64 = Base64.getEncoder

    javaDumpLocation.mkdirs()

    val cLibCRC = {
      val libPath = libraryPaths
        .map(p => (p / s"lib$library.so"))
        .find(_.exists())
        .foreach(lib => md.update(IO.readBytes(lib)))
      base64.encodeToString(md.digest())
    }

    val libMatch =
      if (libSum.exists)
        cLibCRC == base64.encodeToString(IO.readBytes(libSum))
      else false
    val IPaths =
      includePaths.flatMap(f => Seq("-I", f.getCanonicalPath))
    val LPaths =
      libraryPaths.flatMap(f => Seq("-L", f.getCanonicalPath))
    val mappings = packageMappings.toSeq.flatMap {
      case (loc, pack) =>
        Seq("--package-map", s"${loc.getCanonicalPath}=$pack")
    }
    val clangOpts = clangOptions.flatMap(opt => Seq("-C", opt))
    val headerList = headerIncludes.toSeq.map(_.getCanonicalPath)
    val command = headerList ++ mappings ++ IPaths ++ clangOpts ++ LPaths ++
      Seq(
        "--record-library-path",
        "-l",
        library,
        "-t",
        libraryPackage,
        "--src-dump-dir",
        javaDumpLocation.getCanonicalPath
      )
    val cCmdCRC = {
      md.update(command.mkString(" ").getBytes)
      base64.encodeToString(md.digest)
    }

    val commandMatch =
      if (commandSum.exists()) {
        base64.encodeToString(IO.readBytes(commandSum)) == cCmdCRC
      } else
        false

    val cChCrc = {
      recursiveProcessing(javaDumpLocation, logger)(
        (f, n, _) => md.update(IO.readBytes(f / s"${n}_h.java"))
      )
      base64.encodeToString(md.digest())
    }

    val cacheMatch =
      if (cacheSum.exists()) {
        base64.encodeToString(IO.readBytes(cacheSum)) == cChCrc
      } else false

    if (!cacheMatch || !commandMatch || !libMatch) {
      logger.log.debug(s"cacheMatch $cacheMatch")
      logger.log.debug(s"commandMatch $commandMatch")
      logger.log.debug(s"libMatch $libMatch")
      logger.log.debug("refreshing java binding")
      JavaTool("jextract", command: _*).fold(sys.error, {
        case (out, err) =>
          logger.log.debug(err)
          logger.log.info(out)
      })
      recursiveProcessing(javaDumpLocation, logger)(
        (f, n, l) => md.update(IO.readBytes(f / s"${n}_h.java"))
      )
      val decoder = Base64.getDecoder
      IO.write(cacheSum, md.digest())
      IO.write(commandSum, decoder.decode(cCmdCRC))
      IO.write(libSum, decoder.decode(cLibCRC))
    }
    recursiveProcessing(javaDumpLocation, logger)(
      (f, n, _) => f / s"${n}_h.java"
    ) -> (cacheMatch && commandMatch && libMatch)
  }
}
