package io.github.markehammons.shackle.ast.printer

case class File(name: String, pkg: Package, body: Seq[Printable])
