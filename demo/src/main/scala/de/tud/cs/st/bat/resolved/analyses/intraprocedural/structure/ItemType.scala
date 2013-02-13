package de.tud.cs.st.bat.resolved.analyses.intraprocedural.structure

import de.tud.cs.st.bat.resolved.{Type, BaseType, ReferenceType}

/**
 * Abstract class for types of items. An item type holds the information that Type holds but has additional types for null, none and any.
 */
abstract class ItemType {
  def getSize: Int

  def isUpperBoundOf(t: ItemType): Boolean

  def isOfType(t: Type): Boolean

  def isArrayType: Boolean

  def isReferenceType: Boolean
}

/*
 * Lattice of types:
 *
 * none -> null -> someref -> anyref -> any
 *      ->    somebase               ->
 */
object ItemType {

  def fromType(t: Type): ItemType = {
    if (t.isReferenceType)
      SomeRef(t.asInstanceOf[ReferenceType])
    else
      SomeBase(t.asInstanceOf[BaseType])
  }

  def upperBound(a: ItemType, b: ItemType): ItemType = {
    if (a.isUpperBoundOf(b)) {
      a
    } else if (b.isUpperBoundOf(a)) {
      b
    } else {
      ItemType.Any
    }
  }

  /**
   *
   */
  case object Null extends ItemType {

    def getSize: Int = {
      1
    }

    def isUpperBoundOf(t: ItemType): Boolean = {
      return (t == None || t == Null)
    }

    def isOfType(t: Type): Boolean = t.isReferenceType

    def isArrayType: Boolean = false

    def isReferenceType: Boolean = true

    override def toString(): String = "Null"

  }

  /**
   *
   */
  case object None extends ItemType {
    def getSize: Int = {
      1
    }

    def isUpperBoundOf(t: ItemType): Boolean = {
      return (t == None)
    }

    def isOfType(t: Type): Boolean = false

    def isArrayType: Boolean = false

    def isReferenceType: Boolean = false

    override def toString(): String = "None"
  }

  /**
   *
   * @param refType
   */
  case class SomeRef(refType: ReferenceType) extends ItemType {
    def getSize: Int = {
      1
    }

    def isUpperBoundOf(t: ItemType): Boolean = {
      return (t == None || t == Null || t.isInstanceOf[SomeRef])
    }

    def isOfType(t: Type): Boolean = refType.equals(t)

    def isArrayType: Boolean = refType.isArrayType

    def isReferenceType: Boolean = true

    override def toString(): String = refType.toJava
  }

  /**
   *
   */
  case object AnyRef extends ItemType {
    def getSize: Int = {
      1
    }

    def isUpperBoundOf(t: ItemType): Boolean = {
      return (t == None || t == Null || t.isInstanceOf[SomeRef] || t == AnyRef)
    }

    def isOfType(t: Type): Boolean = t.isReferenceType

    def isArrayType: Boolean = false

    def isReferenceType: Boolean = true

    override def toString(): String = "AnyRef"
  }

  /**
   *
   * @param baseType
   */
  case class SomeBase(baseType: BaseType) extends ItemType {
    def getSize: Int = {
      baseType.computationalType.operandSize
    }

    def isUpperBoundOf(t: ItemType): Boolean = {
      return (t == None || t.isInstanceOf[SomeBase])
    }

    def isOfType(t: Type): Boolean = baseType.equals(t)

    def isArrayType: Boolean = false

    def isReferenceType: Boolean = false

    override def toString(): String = baseType.toJava
  }

  /**
   *
   */
  case object Any extends ItemType {
    def getSize: Int = {
      1
    }

    def isUpperBoundOf(t: ItemType): Boolean = {
      return true
    }

    def isOfType(t: Type): Boolean = false

    def isArrayType: Boolean = false

    def isReferenceType: Boolean = false

    override def toString(): String = "Any"
  }

}