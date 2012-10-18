/* License (BSD Style License):
*  Copyright (c) 2009, 2011
*  Software Technology Group
*  Department of Computer Science
*  Technische Universität Darmstadt
*  All rights reserved.
*
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*
*  - Redistributions of source code must retain the above copyright notice,
*    this list of conditions and the following disclaimer.
*  - Redistributions in binary form must reproduce the above copyright notice,
*    this list of conditions and the following disclaimer in the documentation
*    and/or other materials provided with the distribution.
*  - Neither the name of the Software Technology Group or Technische
*    Universität Darmstadt nor the names of its contributors may be used to
*    endorse or promote products derived from this software without specific
*    prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
*  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
*  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
*  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
*  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
*  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
*  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
*  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
*  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
*  POSSIBILITY OF SUCH DAMAGE.
*/
package de.tud.cs.st.bat.resolved

import scala.annotation.tailrec

/**
  * The computational type category of a value on the operand stack. (cf. JVM Spec. 2.11.1 Types and the Java Virtual
  * Machine).
  */
sealed abstract class ComputationalTypeCategory(val operandSize: Byte) {
    def id: Byte
}
final case object Category1ComputationalTypeCategory extends ComputationalTypeCategory(1) {
    def id = 1
}
final case object Category2ComputationalTypeCategory extends ComputationalTypeCategory(2) {
    def id = 2
}

/**
  * The computational type of a value on the operand stack. (cf. JVM Spec. 2.11.1 Types and the Java Virtual
  * Machine).
  */
sealed abstract class ComputationalType(val computationTypeCategory: ComputationalTypeCategory) {
    def operandSize = computationTypeCategory.operandSize
    def isPrimitiveType: Boolean
}
case object ComputationalTypeInt extends ComputationalType(Category1ComputationalTypeCategory) {
    def isPrimitiveType = true
}
case object ComputationalTypeFloat extends ComputationalType(Category1ComputationalTypeCategory) {
    def isPrimitiveType = true
}
case object ComputationalTypeReference extends ComputationalType(Category1ComputationalTypeCategory) {
    def isPrimitiveType = true
}
case object ComputationalTypeReturnAddress extends ComputationalType(Category1ComputationalTypeCategory) {
    def isPrimitiveType = false
}
case object ComputationalTypeLong extends ComputationalType(Category2ComputationalTypeCategory) {
    def isPrimitiveType = true
}
case object ComputationalTypeDouble extends ComputationalType(Category2ComputationalTypeCategory) {
    def isPrimitiveType = true
}

/**
  * A JVM type.
  *
  * From the JVM specification:
  * There are three kinds of reference types: class types, array types, and interface types. Their values are
  * references to dynamically created class instances, arrays, or class instances or arrays that implement
  * interfaces, respectively.
  *
  * An array type consists of a component type with a single dimension (whose length is not given by the
  * type). The component type of an array type may itself be an array type. If, starting from any array type,
  * one considers its component type, and then (if that is also an array type) the component type of that type,
  * and so on, eventually one must reach a component type that is not an array type; this is called the element
  * type of the array type. The element type of an array type is necessarily either a primitive type, or a
  * class type, or an interface type.
  *
  * A reference value may also be the special null reference, a reference to no object, which will be denoted
  * here by null. The null reference initially has no runtime type, but may be cast to any type. The default
  * value of a reference type is null.
  * The Java virtual machine specification does not mandate a concrete value encoding null.
  *
  * @author Michael Eichberg
  */
sealed trait Type {

    def isFieldType: Boolean = false
    def isBaseType: Boolean = false
    def isReferenceType: Boolean = false

    def isVoidType: Boolean = false
    def isByteType: Boolean = false
    def isCharType: Boolean = false
    def isShortType: Boolean = false
    def isIntegerType: Boolean = false
    def isLongType: Boolean = false
    def isFloatType: Boolean = false
    def isDoubleType: Boolean = false
    def isBooleanType: Boolean = false
    def isArrayType: Boolean = false
    def isObjectType: Boolean = false

    def computationalType: ComputationalType

    def toJava: String
}

final object ReturnType {

    def apply(rt: String): Type = if (rt.charAt(0) == 'V') VoidType else FieldType(rt)

}

sealed trait VoidType extends Type with ReturnTypeSignature {

    // remark: the default implementation of equals and hashCode suits our needs!
    def accept[T](sv: SignatureVisitor[T]): T = sv.visit(this)

    override final def isVoidType = true

    def computationalType: ComputationalType =
        throw new Error("\"void\" values do not have a computational type")

    def toJava: String = "void"

    override def toString() = "VoidType"

}
final case object VoidType extends VoidType

sealed trait FieldType extends Type {

    override final def isFieldType = true
}
/**
  * Factory object to parse field type (descriptors) to get field type objects.
  */
object FieldType {

    def apply(ft: String): FieldType = {
        (ft.charAt(0): @scala.annotation.switch) match {
            case 'B' ⇒ ByteType
            case 'C' ⇒ CharType
            case 'D' ⇒ DoubleType
            case 'F' ⇒ FloatType
            case 'I' ⇒ IntegerType
            case 'J' ⇒ LongType
            case 'S' ⇒ ShortType
            case 'Z' ⇒ BooleanType
            case 'L' ⇒ ObjectType(ft.substring(1, ft.length - 1))
            case '[' ⇒ ArrayType(FieldType(ft.substring(1)))
        }
    }
}

sealed trait ReferenceType extends FieldType {

    override final def isReferenceType = true

    def computationalType = ComputationalTypeReference
}
object ReferenceType {

    def apply(rt: String): ReferenceType = {
        if (rt.charAt(0) == '[')
            ArrayType(FieldType(rt.substring(1)))
        else
            ObjectType(rt);
    }

    def unapply(t: ReferenceType): Boolean = true
}

sealed trait BaseType extends FieldType with TypeSignature {

    override final def isBaseType = true

}

sealed trait ByteType extends BaseType {

    override def isByteType = true

    def computationalType = ComputationalTypeInt

    val atype = 8

    def accept[T](v: SignatureVisitor[T]): T = v.visit(this)

    def toJava: String = "byte"

    override def toString() = "ByteType"

}
final case object ByteType extends ByteType

sealed trait CharType extends BaseType {

    override def isCharType = true

    def computationalType = ComputationalTypeInt

    val atype = 5

    def accept[T](v: SignatureVisitor[T]): T = v.visit(this)

    def toJava: String = "char"

    override def toString() = "CharType"

}
final case object CharType extends CharType

sealed trait DoubleType extends BaseType {

    override def isDoubleType = true

    def computationalType = ComputationalTypeDouble

    val atype = 7

    def accept[T](v: SignatureVisitor[T]): T = v.visit(this)

    def toJava: String = "double"

    override def toString() = "DoubleType"

}
final case object DoubleType extends DoubleType

sealed trait FloatType extends BaseType {

    override def isFloatType = true

    def computationalType = ComputationalTypeFloat

    val atype = 6

    def accept[T](v: SignatureVisitor[T]): T = v.visit(this)

    def toJava: String = "float"

    override def toString() = "FloatType"

}
final case object FloatType extends FloatType

sealed trait ShortType extends BaseType {

    override def isShortType = true

    def computationalType = ComputationalTypeInt

    val atype = 9

    def accept[T](v: SignatureVisitor[T]): T = v.visit(this)

    def toJava: String = "short"

    override def toString() = "ShortType"

}
final case object ShortType extends ShortType

sealed trait IntegerType extends BaseType {

    override def isIntegerType = true

    def computationalType = ComputationalTypeInt

    val atype = 10

    def accept[T](v: SignatureVisitor[T]): T = v.visit(this)

    def toJava: String = "int"

    override def toString() = "IntegerType"

}
final case object IntegerType extends IntegerType

sealed trait LongType extends BaseType {

    override def isLongType = true

    def computationalType = ComputationalTypeLong

    val atype = 11

    def accept[T](v: SignatureVisitor[T]): T = v.visit(this)

    def toJava: String = "long"

    override def toString() = "LongType"

}
final case object LongType extends LongType

sealed trait BooleanType extends BaseType {

    override def isBooleanType = true

    def computationalType = ComputationalTypeInt

    val atype = 4

    def accept[T](v: SignatureVisitor[T]): T = v.visit(this)

    def toJava: String = "boolean"

    override def toString() = "BooleanType"

}
final case object BooleanType extends BooleanType

final class ObjectType private (val className: String) extends ReferenceType {

    override final def isObjectType = true

    override lazy val hashCode = className.hashCode * 43

    override def equals(other: Any): Boolean =
        other match {
            case that: ObjectType ⇒
                equals(that)
            case _ ⇒ false
        }

    def equals(other: ObjectType): Boolean = other.className == this.className

    def simpleName: String = ObjectType.simpleName(className)

    def packageName: String = ObjectType.packageName(className)

    def toJava: String = className.replace('/', '.')

    override def toString = "ObjectType(className=\""+className+"\")"

}
object ObjectType {

    // FIXME potential memory leak...
    private val cache: scala.collection.mutable.Map[String, ObjectType] = scala.collection.mutable.Map()

    /**
      * Factory method to create ObjectTypes.<br />
      * This method makes sure that every class is represented by exactly one object type.
      */
    def apply(className: String) = {
        cache.getOrElseUpdate(className, new ObjectType(className))
    }

    def unapply(ot: ObjectType): Option[String] = Some(ot.className)

    def simpleName(className: String): String = {
        val index = className.lastIndexOf('/')
        if (index > -1)
            className.substring(index + 1)
        else
            className
    }

    def packageName(className: String): String = {
        val index = className.lastIndexOf('/')
        if (index == -1)
            ""
        else
            className.substring(0, index)
    }

    val Object = ObjectType("java/lang/Object")
    val String = ObjectType("java/lang/String")
    val Class = ObjectType("java/lang/Class")
    val Throwable = ObjectType("java/lang/Throwable")
    val Error = ObjectType("java/lang/Error")
    val Exception = ObjectType("java/lang/Exception")
    val RuntimeException = ObjectType("java/lang/RuntimeException")
    val IndexOutOfBoundsException = ObjectType("java/lang/IndexOutOfBoundsException")

    // Exceptions and errors that may be throw by the JVM (i.e., instances of these exceptions may be 
    // created at runtime by the JVM)
    val ExceptionInInitializerError = ObjectType("java/lang/ExceptionInInitializerError")

    val NullPointerException = ObjectType("java/lang/NullPointerException")
    val ArrayIndexOutOfBoundsException = ObjectType("java/lang/ArrayIndexOutOfBoundsException")
    val ArrayStoreException = ObjectType("java/lang/ArrayStoreException")
    val NegativeArraySizeException = ObjectType("java/lang/NegativeArraySizeException")
    val IllegalMonitorStateException = ObjectType("java/lang/IllegalMonitorStateException")
    val ClassCastException = ObjectType("java/lang/ClassCastException")
    val ArithmeticException = ObjectType("java/lang/ArithmeticException")

}

final class ArrayType private (val componentType: FieldType) extends ReferenceType {

    override final def isArrayType = true

    override def hashCode = 13 * (componentType.hashCode + 7)

    override def equals(other: Any): Boolean = {
        other match {
            case that: ArrayType ⇒ this.componentType == that.componentType
            case _               ⇒ false
        }
    }

    @scala.deprecated("use \"elementType\" instead", "August 2012") def baseType: Type = elementType

    def elementType: Type = componentType match { case at: ArrayType ⇒ at.elementType; case _ ⇒ componentType }

    def toJava: String = componentType.toJava+"[]"

    override def toString = "ArrayType("+componentType.toString+")"

}
final object ArrayType {

    // FIXME potential memory leak...
    private val cache: scala.collection.mutable.Map[FieldType, ArrayType] = scala.collection.mutable.Map()

    /**
      * Factory method to create objects of type <code>ArrayType</code>.
      *
      * This method makes sure that every array type is represented by exactly one ArrayType object.
      */
    def apply(componentType: FieldType): ArrayType = {
        cache.getOrElseUpdate(componentType, new ArrayType(componentType))
    }

    def apply(dimension: Int, componentType: FieldType): ArrayType = {
        @tailrec
        val at = apply(componentType)
        if (dimension > 1)
            apply(dimension - 1, at)
        else
            at
    }

    def unapply(at: ArrayType): Option[FieldType] = Some(at.componentType)

    def baseType(t: Type): Type = {
        t match {
            case at: ArrayType ⇒ at.elementType
            case _             ⇒ t
        }
    }
}




