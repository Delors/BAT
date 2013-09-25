package de.tud.cs.st.bat.sandbox

/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 17.09.13
 * Time: 13:55
 * To change this template use File | Settings | File Templates.
 */
trait SSAInstruction {
  def isEqual(ssaInstruction : SSAInstruction) : Boolean


  // : Boolean
  def includes(node: SSAInstruction) : Boolean //; checks if an SSA presentation includes a commited SSA presentation
}