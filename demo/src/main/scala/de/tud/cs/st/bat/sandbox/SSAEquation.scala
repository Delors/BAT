package de.tud.cs.st.bat.sandbox

/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 17.09.13
 * Time: 13:57
 * To change this template use File | Settings | File Templates.
 */
abstract class SSAEquation(val y: SSAVariable, val fi: SSAInstruction, val si: SSAInstruction) extends SSAInstruction {

  def getVar() : SSAVariable ={
    y
  }

  /*
  override def isEqual(ssaInstruction: SSAInstruction): Boolean = {
    this == ssaInstruction
  } */

  /*override def includes(node: SSAInstruction): Boolean = node match {
    case SSAConstant(v) => r.includes(node)
    case SSAVariable(v) => r.includes(node)
    //case SSAEquation(va, vb) => r.includes(node)
    case SSAAdd(va, vb, vc) => r.includes(node)
    case SSASub(va, vb, vc) => r.includes(node)
    case SSAMul(va, vb, vc) => r.includes(node)
    case SSAPhi(va, vb, vc) => r.includes(node)
  }*/
}
