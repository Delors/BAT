package de.tud.cs.st.bat.sandbox

/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 17.09.13
 * Time: 13:59
 * To change this template use File | Settings | File Templates.
 */
case class SSAPhi(x: SSAVariable, lop: SSAInstruction, rop: SSAInstruction) extends SSAEquation(x, lop, rop) {
  Memory.insert(x, this)
  //super.y = x
  //super.fi = lop
  //super.si = rop

  override def isEqual(ssaInstruction: SSAInstruction): Boolean = {
    this == ssaInstruction
  }

  override def includes(node: SSAInstruction): Boolean =  node match{
    case SSAConstant(v) => lop.includes(node) || rop.includes(node)
    case SSAVariable(x) => lop.includes(node) || rop.includes(node)
    //case SSAEquation(x, c) => lop.includes(node) || rop.includes(node)
    case SSAAdd(va, vb, vc) => lop.includes(node) || rop.includes(node)
    case SSASub(va, vb, vc) => lop.includes(node) || rop.includes(node)
    case SSAMul(va, vb, vc) => lop.includes(node) || rop.includes(node)
    case SSAPhi(va, vb, vc) => lop.includes(node) || rop.includes(node) || (lop.includes(vb) && rop.includes(vc)) || (lop.includes(vc) && rop.includes(vb))
    case _ => throw new IllegalArgumentException(node.getClass + " is not supported as argument of includes!")
  }
}
