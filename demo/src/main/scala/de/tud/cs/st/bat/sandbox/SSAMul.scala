package de.tud.cs.st.bat.sandbox

/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 17.09.13
 * Time: 13:59
 * To change this template use File | Settings | File Templates.
 */
case class SSAMul(x: SSAVariable, lhs: SSAInstruction, rhs: SSAInstruction) extends SSAEquation(x, lhs, rhs) {
  Memory.insert(x, this)
  //super.y = x
  //super.fi = lhs
  //super.si = rhs

  override def isEqual(ssaInstruction: SSAInstruction): Boolean = {
    this == ssaInstruction
  }

  override def includes(node: SSAInstruction): Boolean = node match {
    case SSAConstant(v) => false
    case SSAVariable(v) => !Memory.isAtom(node.asInstanceOf[SSAVariable]) && SSAMul(x, lhs, rhs).includes(Memory.getValue(node.asInstanceOf[SSAVariable]))
    //case SSAEquation(va, vb) => SSAMul(x, lhs, rhs).includes(vb)
    case SSAAdd(va, vb, vc) => false
    case SSASub(va, vb, vc) => false
    case SSAMul(va, vb, vc) => (lhs.includes(vb) && rhs.includes(vc)) || (rhs.includes(vb) && lhs.includes(vc))
    case SSAPhi(va, vb, vc) => false
    case _ => throw new IllegalArgumentException(node.getClass + " is not supported as argument of varMatching!")
  }
}
