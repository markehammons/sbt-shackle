package io.github.markehammons.shackle.ast.printer

trait Printable {
  def toDotty(): Either[Exception, Seq[Emittable]]
}
