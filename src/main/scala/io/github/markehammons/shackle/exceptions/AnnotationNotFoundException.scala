package io.github.markehammons.shackle.exceptions

import com.github.javaparser.ast.body.{
  BodyDeclaration,
  ClassOrInterfaceDeclaration,
  MethodDeclaration
}

class AnnotationNotFoundException(message: String) extends Exception(message)

object AnnotationNotFoundException {
  def fromDeclaration(
      bd: BodyDeclaration[_],
      annotationName: String
  ): AnnotationNotFoundException = {
    val message = bd match {
      case coi: ClassOrInterfaceDeclaration =>
        s"@$annotationName not found as part of class declaration ${coi.getName}"
      case md: MethodDeclaration =>
        s"@$annotationName not found as part of method declaration $md"
    }

    new AnnotationNotFoundException(message)
  }
}
