package io.github.markehammons.sbt_jextract

import com.github.javaparser.ast.expr.Name

import scala.annotation.tailrec
import scala.compat.java8.OptionConverters._

case class ImportAst(path: List[String], wildcard: Boolean) {
  override def toString: String = s"${path.mkString(".")}${if(wildcard) "._"}"
}

object ImportAst {
  def pathFromName(name: Name): List[String] = {
    @tailrec
    def helper(name: Name, path: List[String]): List[String] = {
      val res = name.getIdentifier :: path
      name.getQualifier.asScala match {
        case Some(n) => helper(n, res)
        case _ => res
      }
    }
    helper(name, Nil)
  }
}