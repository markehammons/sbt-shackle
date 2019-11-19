package io.github.markehammons.shackle.ast

import io.github.markehammons.shackle.ast.printer.{Literal, Renderable}

//todo: Signature should parse the jextract signature
case class Signature(string: String) extends Literal with Renderable {
  override def asDottyString(): String = string
}
