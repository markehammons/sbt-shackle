package io.github.markehammons.shackle.ast.printer

sealed trait AccessModifier extends Renderable {
  override def asDottyString(): String = this match {
    case Priv => "private "
    case Prot => "protected "
    case _    => ""
  }
}

case object Priv extends AccessModifier
case object Prot extends AccessModifier
case object Pub extends AccessModifier
