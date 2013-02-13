package de.tud.cs.st.bat.resolved.analyses.intraprocedural.structure

import de.tud.cs.st.bat.resolved._


/**
 * Created with IntelliJ IDEA.
 * User: Mirko
 * Date: 29.11.12
 * Time: 15:22
 * To change this template use File | Settings | File Templates.
 */
//TODO: implement one function for push and pop
case class Stack(maxSize: Int, values: List[Item]) {

  def size: Int = {
    values.size
  }

  def apply(index: Int): Item = {
    values(index)
  }

  def get(index: Int): Item = {
    var listIndex = 0
    var currentIndex = 0

    while (currentIndex < index) {
      currentIndex = currentIndex + 1
      listIndex = listIndex + values(listIndex).size
    }

    return values(listIndex)
  }

  def getSlot(index: Int): Item = {
    values(index)
  }

  def push(t: Item): Stack = {
    if (t == null)
      return this
    if (size + t.size > maxSize)
      return this

    var res = values

    for (i <- 1 until t.size) {
      res = Item.createContinue(t) :: res
    }

    res = t :: res
    return Stack(maxSize, res)
  }


  def push(t: Type, pc: Int): Stack = {
    push(Item.createItem(ItemType.fromType(t), pc))
  }

  def pop(amount: Int): Stack = {
    if (values.size < amount)
      return this

    var res = values
    for (i <- 1 to amount) {
      res = res.tail
    }

    return Stack(maxSize, res)
  }

  def pop(): Stack = {
    if (values == Nil)
      return this

    pop(values.head.size)
  }


  def swap(): Stack = {
    if (size < 2)
      return this

    return Stack(maxSize, values(1) :: values(0) :: values.drop(2))

  }

  def dup(amount: Int, offset: Int): Stack = {
    if (size + amount + offset > maxSize)
      return this

    val duplicate = values.take(amount)

    return Stack(maxSize, duplicate ++ values.slice(amount, amount + offset) ++ duplicate ++ values.drop(amount + offset))
  }

  override def toString(): String = {
    return values.mkString("[", "/", "]")
  }


}
