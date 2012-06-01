/* License (BSD Style License):
*  Copyright (c) 2009, 2011
*  Software Technology Group
*  Department of Computer Science
*  Technische Universität Darmstadt
*  All rights reserved.
*
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*
*  - Redistributions of source code must retain the above copyright notice,
*    this list of conditions and the following disclaimer.
*  - Redistributions in binary form must reproduce the above copyright notice,
*    this list of conditions and the following disclaimer in the documentation
*    and/or other materials provided with the distribution.
*  - Neither the name of the Software Technology Group or Technische
*    Universität Darmstadt nor the names of its contributors may be used to
*    endorse or promote products derived from this software without specific
*    prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
*  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
*  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
*  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
*  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
*  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
*  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
*  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
*  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
*  POSSIBILITY OF SUCH DAMAGE.
*/
package de.tud.cs.st.bat.resolved.analyses
import de.tud.cs.st.bat.resolved.ClassFile
import de.tud.cs.st.bat.resolved.ObjectType
import de.tud.cs.st.bat.resolved.Method
import de.tud.cs.st.bat.resolved.INVOKESTATIC
import de.tud.cs.st.bat.resolved.LDC

class AuthAnalyser
object AuthAnalyser extends AuthAnalyser {

	def checkForPasswords(classFile : ClassFile) = {
		//		println(classFile.thisClass.className)
//		if (classFile.thisClass.className.equals("org/apache/activemq/ActiveMQConnectionFactory")) debug(classFile)
		var typicalPasswords : List[(String, String)] = Nil

		val passwords = List("pw", "pwd", "pass", "password", "passwort", "passfield")
		val passwordTypes = List(ObjectType("java/lang/String"), ObjectType("java/awt/TextField"), ObjectType("javax/swing/JTextField"))

		classFile.fields.foreach(field =>
			if (passwords.contains(field.name.toLowerCase()) && passwordTypes.contains(field.fieldType)) typicalPasswords = (classFile.thisClass.className, "") :: typicalPasswords)

		for (method ← classFile.methods; if !method.body.isEmpty) {
			val table = method.body.map(_.localVariableTable)
			table match {
				case Some(Some(x)) => x.foreach(local => if (passwords.contains(local.name) && passwordTypes.contains(local.fieldType)) {
					typicalPasswords = (classFile.thisClass.className, method.name) :: typicalPasswords
				})

				case _ =>
			}
		}
//		if (classFile.thisClass.className.equals("org/apache/activemq/ActiveMQConnectionFactory"))typicalPasswords.foreach(println)
		
		typicalPasswords
	}

	def hardCodedSQLCredentials(classFile : ClassFile) = {
		debug(classFile)
		var sqlConnections : List[(ClassFile, Method)] = Nil

		val driverManager = ObjectType("java/sql/DriverManager")
		val string = ObjectType(className = "java/lang/String")

		for (method ← classFile.methods; if !method.body.isEmpty && !method.body.get.instructions.isEmpty) {
			var constantString, connection = false
			for (instruction ← method.body.get.instructions) {
				instruction match {
					case is : INVOKESTATIC => if (is.name.equals("getConnection") && is.declaringClass.equals(driverManager)) connection = true
					case ldc : LDC => if (ldc.constantValue.valueType.equals(string)) constantString = true
					case _ =>
				}

			}
			if (constantString && connection) sqlConnections = (classFile, method) :: sqlConnections
		}
		sqlConnections
	}

	private def debug(classFile : ClassFile) : Unit = {
		/*
	   * For Source code purposes
	   */

		println(classFile.thisClass.className)

		for (method ← classFile.methods) {
			println(method.name)
			val table = method.body.map(_.localVariableTable)
			table match {
				case Some(Some(x)) => x.foreach(local => println(local.name))
				case _ =>
			}
			var line = 0
			for (instruction ← method.body.get.instructions if !method.body.get.instructions.isEmpty) {

				if (instruction != null) println("\t" + line + " " + instruction)
				line += 1
			}
		}

		println
	}
}