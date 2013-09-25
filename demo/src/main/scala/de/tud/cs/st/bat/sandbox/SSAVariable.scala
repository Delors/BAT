package de.tud.cs.st.bat.sandbox

/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 17.09.13
 * Time: 13:54
 * To change this template use File | Settings | File Templates.
 */
case class SSAVariable(id: String) extends SSAPrimitive {
  Memory.insert(this) //TODO Nachschauen, ob immer ausgeführt oder wirklich nur Constructor

  override def isEqual(ssaInstruction: SSAInstruction): Boolean = {
    this == ssaInstruction
  }
  //Immer checken ob Variable atomar, wenn value von einer Variablen nachgefragt wird
  override def includes(node: SSAInstruction): Boolean = node match {
    case SSAConstant(v) => (!Memory.isAtom(SSAVariable(id)) && Memory.getValue(SSAVariable(id)).includes(node)) //TODO Als Beispiel, reicht aus, um Endlosschleife zu verhindern?
    case SSAVariable(v) => SSAVariable(id).isEqual(node.asInstanceOf[SSAVariable]) || (!Memory.isAtom(SSAVariable(id)) && Memory.getValue(SSAVariable(id)).includes(node))
    //case SSAEquation(va, vb) => SSAVariable(id).isEqual(va.asInstanceOf[SSAVariable]) || SSAVariable(id).isEqual(vb.asInstanceOf[SSAVariable]) || (!Memory.isAtom(SSAVariable(id)) && Memory.getValue(SSAVariable(id)).includes(vb))
    case SSAAdd(va, vb, vc) => SSAVariable(id).isEqual(va.asInstanceOf[SSAVariable]) || (!Memory.isAtom(SSAVariable(id)) && Memory.getValue(SSAVariable(id)).includes(node))
    case SSASub(va, vb, vc) => SSAVariable(id).isEqual(va.asInstanceOf[SSAVariable]) || (!Memory.isAtom(SSAVariable(id)) && Memory.getValue(SSAVariable(id)).includes(node))
    case SSAMul(va, vb, vc) => SSAVariable(id).isEqual(va.asInstanceOf[SSAVariable]) || (!Memory.isAtom(SSAVariable(id)) && Memory.getValue(SSAVariable(id)).includes(node))
    case SSAPhi(va, vb, vc) => SSAVariable(id).isEqual(va.asInstanceOf[SSAVariable]) || (!Memory.isAtom(SSAVariable(id)) && Memory.getValue(SSAVariable(id)).includes(node))
    case _ => throw new IllegalArgumentException(node.getClass + " is not supported as argument of varMatching!")
  }
}