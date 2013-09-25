package de.tud.cs.st.bat.sandbox
import scala.collection.JavaConverters._

/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 28.05.13
 * Time: 12:41
 * To change this template use File | Settings | File Templates.
 */
object Memory {

  /* val set = scala.collection.mutable.ArrayBuffer.empty[SSAInstruction]
  var i = 0*/

  var lookuptable = scala.collection.immutable.HashMap.empty[SSAVariable, SSAInstruction]

  //TODO: Unterschied insert, change herausarbeiten
  def insert(x : SSAVariable, value : SSAInstruction) = {
    if (!lookuptable.contains(x)){
      lookuptable += (x -> value)
    } else{
      lookuptable.-(x)
      lookuptable += (x -> value)
    }

  }

  def insert(x : SSAVariable) = {
    if(!lookuptable.contains(x)){
    lookuptable += (x -> x)}
  }

  def getValue(x:SSAVariable) : SSAInstruction = {
    lookuptable(x) //TODO Schleife mit Abbruch bei atomaren Elementen ???
  }

  //atom means: the variable only stands for itself and has no other value
  def isAtom(x : SSAVariable) : Boolean = {
    x == lookuptable(x)
  }

  def change(x : SSAVariable, value : SSAInstruction) = {
    if (lookuptable.contains(x)){
      lookuptable.-(x)
      lookuptable += (x -> value)
    } else{
      insert(x, value)
    }

  }

  def getValues() : Iterable[SSAInstruction] ={
    lookuptable.values
  }
  /*
  def add(instruction : SSAInstruction) = {
    set.insert(i, instruction)
    i+=1
  }
  def tell() = {
    val list = set.toArray
    for {ssa <- list
      if i != 0
  } yield{
      MyClient.myMatching(ssa)
    }
 }
 */


}
