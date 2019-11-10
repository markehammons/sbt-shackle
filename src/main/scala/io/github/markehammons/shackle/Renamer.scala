package io.github.markehammons.shackle

import io.github.markehammons.shackle.ast.Header

object Renamer {
  def apply(ast: Header, renameLevel: RenameLevel) = {
    renameLevel match {
      case Minimal => minimalPhase(ast)
    }
  }

  val underscoreRegex = """(.*_)""".r
  def minimalPhase(ast: Header): Header = {
    val renameFunction = (s: String) =>
      s match {
        case "class"                  => "class$"
        case "type"                   => "type$"
        case "new"                    => "new$"
        case "object"                 => "object$"
        case "notify"                 => "notify$"
        case "match"                  => "match$"
        case str if str.endsWith("_") => s"$str$$"
        case str                      => str
      }
    ast.copy(
      structs = ast.structs.map(
        s =>
          s.copy(members = s.members.map { m =>
            val phase1 = renameFunction(m.name)
            m.copy(name = phase1)
          })
      ),
      numericConstants = ast.numericConstants.map { nc =>
        val phase1 = renameFunction(nc.name)
        nc.copy(name = phase1)
      },
      nativeStringConstants = ast.nativeStringConstants.map { nsc =>
        val phase1 = renameFunction(nsc.name)
        nsc.copy(name = phase1)
      },
      functions = ast.functions.map(
        func =>
          func.copy(parameters = func.parameters.map { p =>
            val phase1 = renameFunction(p.name)
            p.copy(name = phase1)
          })
      )
    )
  }
}
