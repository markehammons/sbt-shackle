package io.github.markehammons.shackle.printer

import io.github.markehammons.shackle.Lang

trait MinimalPrinter[L <: Lang] extends Printer[L] {
  def toAst
}
