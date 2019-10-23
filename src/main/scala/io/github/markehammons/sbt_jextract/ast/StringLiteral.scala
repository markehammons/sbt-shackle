package io.github.markehammons.sbt_jextract.ast

case class StringLiteral(value: String) {
  override def toString = "\"" + value + "\""
}
