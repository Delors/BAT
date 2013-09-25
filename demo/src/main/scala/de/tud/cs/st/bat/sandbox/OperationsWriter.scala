package de.tud.cs.st.bat.sandbox

import de.tud.cs.st.bat.resolved.{IDIV, IADD, Instruction}

/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 21.05.13
 * Time: 14:20
 * To change this template use File | Settings | File Templates.
 */
object OperationsWriter {

  def write(instruction : Instruction) : String ={
    instruction match {
      case IADD =>
      case IDIV =>
    }

    instruction.opcode match {
      case 232 =>
    }

    var s : StringBuilder = new StringBuilder
    if (instruction == IADD){
        s.append(VariableMaker.newVar())
        s.append(" + ")
        s.append(VariableMaker.newVar())
    }
    if (instruction == IDIV){
      s.append(VariableMaker.newVar())
      s.append(" / ")
      s.append(VariableMaker.newVar())
    }
    return s.toString
  }

}
