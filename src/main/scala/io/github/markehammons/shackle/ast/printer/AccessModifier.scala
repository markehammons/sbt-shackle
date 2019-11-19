package io.github.markehammons.shackle.ast.printer

sealed trait AccessModifier

case object Priv extends AccessModifier
case object Prot extends AccessModifier
case object Pub extends AccessModifier
