package de.tud.cs.st.bat.sandbox

/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 21.05.13
 * Time: 12:49
 * To change this template use File | Settings | File Templates.
 */
object VariableMaker {

  var i: Int = 0

  def newVar(): String = {
    var s: StringBuilder = new StringBuilder()
    s.append("x")
    s.append(i)
    i +=1
    return s.toString()
  }
}
