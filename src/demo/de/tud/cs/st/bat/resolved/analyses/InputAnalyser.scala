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
import de.tud.cs.st.bat.resolved.GETSTATIC
import de.tud.cs.st.bat.resolved.INVOKEVIRTUAL
import de.tud.cs.st.bat.resolved.Method
import de.tud.cs.st.bat.resolved.ObjectType
import de.tud.cs.st.bat.resolved.INVOKEINTERFACE
import de.tud.cs.st.bat.resolved.INVOKESPECIAL
import de.tud.cs.st.bat.resolved.INVOKESTATIC

class InputAnalyser
object InputAnalyser extends InputAnalyser {

	def specialElementsInCommand(classFile : ClassFile) = {

		var specialCommand : List[(ClassFile, Method)] = Nil
		val runtime = ObjectType("java/lang/Runtime")
		val stringbuilder = ObjectType("java/lang/StringBuilder")

		for (method ← classFile.methods; if !method.body.isEmpty && !method.body.get.instructions.isEmpty) {
			var getParameter, stringConcat, execute = false
			for (instruction ← method.body.get.instructions) {
				instruction match {
					case ii : INVOKEINTERFACE => if (ii.name.equals("getParameter")) getParameter = true
					case iv : INVOKEVIRTUAL =>
						if (iv.name.equals("append") && iv.declaringClass.equals(stringbuilder)) {
							stringConcat = true
						} else if (iv.name.equals("exec") && iv.declaringClass.equals(runtime)) {
							execute = true
						}
					case _ =>
				}

			}
			if (getParameter && stringConcat && execute) specialCommand = (classFile, method) :: specialCommand
		}
		specialCommand
	}

	def pathTraversal(classFile : ClassFile) = {
		var pathTravers : List[(ClassFile, Method)] = Nil
		val file = ObjectType("java/io/File")
		val stringbuilder = ObjectType("java/lang/StringBuilder")

		for (method ← classFile.methods; if !method.body.isEmpty && !method.body.get.instructions.isEmpty) {
			var getParameter, stringConcat, fileUsage = false
			for (instruction ← method.body.get.instructions) {
				instruction match {
					case ii : INVOKEINTERFACE => if (ii.name.equals("getParameter")) getParameter = true
					case iv : INVOKEVIRTUAL => if (iv.name.equals("append") && iv.declaringClass.equals(stringbuilder)) stringConcat = true
					case is : INVOKESPECIAL => if (is.declaringClass.equals(file)) fileUsage = true
					case _ =>
				}

			}
			if (getParameter && stringConcat && fileUsage) pathTravers = (classFile, method) :: pathTravers
		}
		pathTravers
	}

	def downloadedCode(classFile : ClassFile) = {
		var externalClasses : List[(ClassFile, Method)] = Nil

		val urlClassLoader = ObjectType("java/net/URLClassLoader")
		val classType = ObjectType("java/lang/Class")
		val classLoaderType = ObjectType("java/lang/ClassLoader")

		for (method ← classFile.methods; if !method.body.isEmpty && !method.body.get.instructions.isEmpty) {
			var url, forName, classloader, newInstance = false
			for (instruction ← method.body.get.instructions) {
				instruction match {
					case ii : INVOKEINTERFACE =>
					case iv : INVOKEVIRTUAL =>
						if (iv.name.equals("loadClass") && iv.declaringClass.equals(classLoaderType)) {
							classloader = true
						} else if (iv.name.equals("newInstance") && iv.declaringClass.equals(classType)) {
							newInstance = true
						}
					case ist : INVOKESTATIC => if (ist.name.equals("forName") && ist.declaringClass.equals(classType)) forName = true
					case is : INVOKESPECIAL => if (is.declaringClass.equals(urlClassLoader)) url = true
					case _ =>
				}

			}
			if ((url && forName) || (url && classloader && newInstance)) externalClasses = (classFile, method) :: externalClasses
		}
		externalClasses
	}

	def SQLInjection(classFile : ClassFile) = {
		var sqlInjected : List[(ClassFile, Method)] = Nil
		val statement = ObjectType("java/sql/PreparedStatement")
		val stringbuilder = ObjectType("java/lang/StringBuilder")

		for (method ← classFile.methods; if !method.body.isEmpty && !method.body.get.instructions.isEmpty) {
			var getParameter, stringConcat, executeStatement = false
			for (instruction ← method.body.get.instructions) {
				instruction match {
					case ii : INVOKEINTERFACE =>
						if (ii.name.equals("getParameter")) {
							getParameter = true
						} else if(ii.name.equals("executeQuery")&& ii.declaringClass.equals(statement)){
							executeStatement = true	
						}
					case iv : INVOKEVIRTUAL => if (iv.name.equals("append") && iv.declaringClass.equals(stringbuilder)) stringConcat = true
					case _ =>
				}

			}
			if (getParameter && stringConcat && executeStatement) sqlInjected = (classFile, method) :: sqlInjected
		}
		sqlInjected
	}
	
	def uncheckedRedirection(classFile : ClassFile) = {
//		debug(classFile)
		
		var redirected : List[(ClassFile, Method)] = Nil
		val string= ObjectType("java/lang/String")
		val request = ObjectType("javax/servlet/http/HttpServletRequest")
		val response = ObjectType("javax/servlet/http/HttpServletResponse")

		for (method ← classFile.methods; if !method.body.isEmpty && !method.body.get.instructions.isEmpty) {
			var getParameter, redirect,change = false
			for (instruction ← method.body.get.instructions) {
				instruction match {
					case ii : INVOKEINTERFACE =>
						if (ii.name.equals("getParameter") && ii.declaringClass.eq(request)) {
							getParameter = true
						} else if(ii.name.equals("sendRedirect")&& ii.declaringClass.equals(response)){
							redirect = true	
						}
					case iv : INVOKEVIRTUAL => if(iv.declaringClass.equals(string))change = true
					case _ =>
				}

			}
			if (getParameter && redirect && !change) redirected = (classFile, method) :: redirected
		}
		redirected
	}

	private def debug(classFile : ClassFile) : Unit = {
		/*
	   * For Source code purposes
	   */

		println(classFile.thisClass.className)

		for (method ← classFile.methods) {
			println(method.name)
			var line = 0
			for (instruction ← method.body.get.instructions if !method.body.get.instructions.isEmpty) {

				if (instruction != null) println("\t" + line + " " + instruction)
				line += 1
			}
		}

		println
	}
}