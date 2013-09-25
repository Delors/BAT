package de.tud.cs.st.bat.sandbox

import de.tud.cs.st.bat.resolved.{GETFIELD, INVOKESPECIAL, ALOAD_0, Instruction}
/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 04.06.13
 * Time: 12:10
 * To change this template use File | Settings | File Templates.
 */
object Booleancheck {

  def checkUnsupported(instruction : Instruction) : Boolean ={
    if (instruction == INVOKESPECIAL)
      return true
    return false
  }

  def checkProcessing(instruction : Instruction) : Boolean ={
    if (instruction == GETFIELD)
      return true
    return false
  }

  def checkVariable(instruction : Instruction) : Boolean ={
    if (instruction == ALOAD_0)
      return true
    return false
  }

}
