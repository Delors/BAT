package de.tud.cs.st.bat.sandbox
import de.tud.cs.st.bat.resolved.{IDIV, IADD, Instruction, ClassFile}

/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 21.05.13
 * Time: 13:09
 * To change this template use File | Settings | File Templates.
 */
object BasicOperationsAnalysis {
     def check(instruction : Instruction) : Boolean = {
       (instruction == IADD || instruction == IDIV)

     }
}
