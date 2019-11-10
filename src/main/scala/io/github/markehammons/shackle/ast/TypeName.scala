package io.github.markehammons.shackle.ast

import scala.compat.java8.OptionConverters._
import scala.collection.JavaConverters._
import com.github.javaparser.ast.`type`.{
  ClassOrInterfaceType,
  PrimitiveType,
  Type,
  VoidType,
  WildcardType => typ_?
}

sealed trait TypeAst {

  /**
    * List of class names
    * @return the class names represented by this abstract syntax tree
    */
  def classNames: List[ClassName]

  def asDottyString: String
}

object TypeAst {
  def fromType(t: Type): TypeAst = {
    t match {
      case primitive: PrimitiveType =>
        primitive.getType.toString match {
          case "INT"     => IntType
          case "LONG"    => LongType
          case "SHORT"   => ShortType
          case "BYTE"    => ByteType
          case "BOOLEAN" => BooleanType
          case "FLOAT"   => FloatType
          case "DOUBLE"  => DoubleType
        }

      case _: VoidType => UnitType
      case _: typ_?    => WildcardType
      case classTyp: ClassOrInterfaceType
          if classTyp.getName.asString == "Void" =>
        UnitType
      case classType: ClassOrInterfaceType
          if classType.getName.asString() == "Byte" =>
        ByteType
      case classType: ClassOrInterfaceType
          if classType.getName.asString() == "Integer" =>
        IntType
      case classType: ClassOrInterfaceType
          if classType.getName.asString() == "Long" =>
        LongType
      case classTyp: ClassOrInterfaceType
          if classTyp.getTypeArguments.asScala.isDefined && classTyp.getName
            .asString() == "Pointer" =>
        PointerType(
          TypeAst.fromType(classTyp.getTypeArguments.get().asScala.head)
        )
      case classTyp: ClassOrInterfaceType
          if classTyp.getTypeArguments.asScala.isDefined && classTyp.getName.asString == "Array" =>
        ArrayType(
          TypeAst.fromType(classTyp.getTypeArguments.get().asScala.head)
        )
      case classTyp: ClassOrInterfaceType
          if classTyp.getTypeArguments.asScala.isDefined && classTyp.getName
            .asString() == "Callback" =>
        CallbackType(
          TypeAst.fromType(classTyp.getTypeArguments.get().asScala.head)
        )
      case classTyp: ClassOrInterfaceType
          if classTyp.getTypeArguments.asScala.isDefined && classTyp.getName
            .asString() == "Struct" =>
        StructType(
          ClassName.fromString(
            classTyp.getTypeArguments.get().asScala.head.toString
          )
        )
      case classTyp: ClassOrInterfaceType
          if classTyp.getTypeArguments.asScala.isEmpty =>
        Clazz(ClassName.fromString(classTyp.toString))
      case t => throw new Exception(s"Unknown type $t")
    }
  }
}

case object IntType extends TypeAst {
  override def classNames: List[ClassName] =
    List(ClassName(Package(Nil), "Int"))

  override def asDottyString: String = "Int"
}

case object LongType extends TypeAst {
  override def classNames: List[ClassName] =
    List(ClassName(Package(Nil), "Long"))

  override def asDottyString: String = "Long"
}

case object ShortType extends TypeAst {
  override def classNames: List[ClassName] =
    List(ClassName(Package(Nil), "Short"))

  override def asDottyString: String = "Short"
}

case object ByteType extends TypeAst {
  override def classNames: List[ClassName] =
    List(ClassName(Package(Nil), "Byte"))

  override def asDottyString: String = "Byte"
}

case object BooleanType extends TypeAst {
  override def classNames: List[ClassName] =
    List(ClassName(Package(Nil), "Boolean"))

  override def asDottyString: String = "Boolean"
}

case object FloatType extends TypeAst {
  override def classNames: List[ClassName] =
    List(ClassName(Package(Nil), "Float"))

  override def asDottyString: String = "Float"
}

case object DoubleType extends TypeAst {
  override def classNames: List[ClassName] =
    List(ClassName(Package(Nil), "Double"))

  override def asDottyString: String = "Double"
}
case object UnitType extends TypeAst {
  override def classNames: List[ClassName] =
    List(ClassName(Package(Nil), "Unit"))

  override def asDottyString: String = "Unit"
}
case object WildcardType extends TypeAst {
  override def classNames: List[ClassName] = Nil

  override def asDottyString: String = "?"
}
case class StructType(className: ClassName) extends TypeAst {
  override def classNames: List[ClassName] =
    List(className, StructType.structClassName)

  override def asDottyString: String =
    s"${StructType.structClassName.asDottyString}[${className.asDottyString}]"
}

object StructType {
  private val structClassName =
    ClassName(Package(List("java", "foreign", "memory")), "Struct")
}

case class PointerType(typeAst: TypeAst) extends TypeAst {
  override def classNames: List[ClassName] =
    PointerType.pointerTypeName :: typeAst.classNames

  override def asDottyString: String =
    s"${PointerType.pointerTypeName.asDottyString}[${typeAst.asDottyString}]"
}

object PointerType {
  private val pointerTypeName =
    ClassName(Package(List("java", "foreign", "memory")), "Pointer")
}

case class ArrayType(typeAst: TypeAst) extends TypeAst {
  val classNames
      : List[ClassName] = ArrayType.arrayTypeName :: typeAst.classNames

  override def asDottyString: String =
    s"${ArrayType.arrayTypeName.asDottyString}[${typeAst.asDottyString}]"
}

object ArrayType {
  private val arrayTypeName =
    ClassName(Package(List("java", "foreign", "memory")), "Array")
}

case class CallbackType(typeAst: TypeAst) extends TypeAst {
  val classNames
      : List[ClassName] = CallbackType.callbackTypeName :: typeAst.classNames

  override def asDottyString: String =
    s"${CallbackType.callbackTypeName.asDottyString}[${typeAst.asDottyString}]"
}

object CallbackType {
  private val callbackTypeName =
    ClassName(Package(List("java", "foreign", "memory")), "Callback")
}

case class Clazz(className: ClassName) extends TypeAst {
  val classNames = List(className)

  override def asDottyString: String = className.asDottyString
}
