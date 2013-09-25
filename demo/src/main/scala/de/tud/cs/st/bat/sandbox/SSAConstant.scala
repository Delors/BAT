package de.tud.cs.st.bat.sandbox

/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 17.09.13
 * Time: 13:56
 * To change this template use File | Settings | File Templates.
 */

case class SSAConstant(value: Int) extends SSAPrimitive {
  override def isEqual(ssaInstruction: SSAInstruction): Boolean =
    this == ssaInstruction

  override def includes(node: SSAInstruction): Boolean = node match {
    case SSAConstant(v) => (SSAConstant(value)).isEqual(node.asInstanceOf[SSAConstant])
    case SSAVariable(v) => false // Da die Konstante die Information der Variablen min. beinhalten müsste: Nein!
    //case SSAEquation(va, vb) => false
    case SSAAdd(va, vb, vc) => false
    case SSASub(va, vb, vc) => false
    case SSAMul(va, vb, vc) => false
    case SSAPhi(va, vb, vc) => false
    case _ => throw new IllegalArgumentException(node.getClass + " is not supported as argument of varMatching!")
  }
}

