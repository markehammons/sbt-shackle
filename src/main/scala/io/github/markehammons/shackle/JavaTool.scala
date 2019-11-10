package io.github.markehammons.shackle

import java.io.{ByteArrayOutputStream, PrintWriter}
import java.util.spi.ToolProvider

object JavaTool {
  def apply(
      name: String,
      arguments: String*
  ): Either[String, (String, String)] = {
    val maybeTool: Option[ToolProvider] = {
      val _tool = ToolProvider.findFirst(name)
      if (_tool.isPresent) {
        Some(_tool.get())
      } else {
        None
      }
    }

    val result = for (tool <- maybeTool) yield {
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
      .toRight(
        s"Could not find tool $name in your java development environment"
      )
      .flatMap {
        case (code, ret, err) =>
          if (code != 0) {
            Left(s"failure with code $code: ${ret + err}")
          } else {
            Right(ret -> err)
          }
      }
  }
}
