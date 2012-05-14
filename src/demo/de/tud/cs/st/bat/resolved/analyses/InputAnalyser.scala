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