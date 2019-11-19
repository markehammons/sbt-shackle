package io.github.markehammons.shackle.printer

import io.github.markehammons.shackle.ast.Header

trait Printer[LANG] {
  def toString(header: Header): String
}
