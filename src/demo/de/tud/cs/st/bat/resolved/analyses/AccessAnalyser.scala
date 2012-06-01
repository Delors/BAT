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

import scala.Array.canBuildFrom
import de.tud.cs.st.bat.resolved.reader.Java6Framework.ClassFile
import de.tud.cs.st.bat.resolved.reader.Java6Framework
import de.tud.cs.st.util.perf.nsToSecs
import de.tud.cs.st.util.perf.Counting
import de.tud.cs.st.util.perf.PerformanceEvaluation
import de.tud.cs.st.bat.resolved.ISTORE_1
import de.tud.cs.st.bat.resolved.ISTORE_1
import de.tud.cs.st.bat.resolved.ISTORE_0
import de.tud.cs.st.bat.resolved.ISTORE_2
import de.tud.cs.st.bat.resolved.ISTORE_3
import de.tud.cs.st.bat.resolved.ISTORE
import de.tud.cs.st.bat.resolved.IFEQ
import de.tud.cs.st.bat.resolved.Method
import de.tud.cs.st.bat.resolved.INVOKESPECIAL
import de.tud.cs.st.bat.resolved.ObjectType
import de.tud.cs.st.bat.resolved.LDC2_W
import de.tud.cs.st.bat.resolved.ConstantLong
import de.tud.cs.st.bat.resolved.ConstantInteger
import de.tud.cs.st.bat.resolved.LDC2_W
import de.tud.cs.st.bat.resolved.GETSTATIC
import de.tud.cs.st.bat.resolved.INVOKESTATIC
import de.tud.cs.st.bat.resolved.MethodDescriptor
import de.tud.cs.st.bat.resolved.IntegerType
import de.tud.cs.st.bat.resolved.INVOKEVIRTUAL
import de.tud.cs.st.bat.resolved.VoidType
import de.tud.cs.st.bat.resolved.BooleanType

/**
 *
 * @author Dennis Siebert
 */
class AccessAnalyser extends Analyser 
object AccessAnalyser extends AccessAnalyser{
	/**
	 * checks for fields which are static and public and not final. In most cases they should be declared final, too.
	 */
	def FieldNotFinal(classFile : ClassFile) = {
		for (
			field ← classFile.fields if field.isPublic && field.isStatic && !field.isFinal
		) yield (classFile, field);
	}

	/**
	 * checks for array fields, which are public static final. The array may be immutable but not its content.
	 */
	def ArrayPSF(classFile : ClassFile) = {
		for (
			field ← classFile.fields if field.fieldType.isArrayType &&
				field.isPublic && field.isStatic && field.isFinal
		) yield (classFile, field)
	}

	/**
	 * checks if a random object is created with a predicatble seed
	 * NOT IMPLEMENTED YET
	 */
	def RandomSeedAnalyser(classFile : ClassFile) = {
		var predictableSeed : List[(ClassFile, Method)] = Nil
		val randomType = ObjectType("java/util/Random")
		val system = ObjectType("java/lang/System")
		for (method ← classFile.methods; if !method.body.isEmpty && !method.body.get.instructions.isEmpty) {
			var constant, millis, random = false
			for (instruction ← method.body.get.instructions) {
				instruction match {
					case is : INVOKESTATIC =>
						if (is.declaringClass.equals(system) && is.name.equals("currentTimeMillis")) millis = true
					case isp : INVOKESPECIAL => if (isp.declaringClass.equals(randomType)) random = true
					case LDC2_W(ConstantLong(_)) => constant = true
					case _ =>
				}
			}
			if ((constant || millis) && random) predictableSeed = (classFile, method) :: predictableSeed
		}
		predictableSeed
	}

	def protectedFields(classFile : ClassFile) = {
		for (
			field ← classFile.fields if field.isProtected && classFile.isFinal
		) yield (classFile, field)
	}

	def unsafeUseOfJNI(classFile : ClassFile) = {

		var JNI : List[(ClassFile, Method)] = Nil
		for (method <- classFile.methods if method.body.isDefined && method.body.get != null) {
			if (method.body.get.instructions.exists({
				case INVOKESTATIC(ObjectType("java/lang/System"), "loadLibrary", _) => true;
				case _ ⇒ false;
			})) {
				JNI = (classFile, method) :: JNI
			}
		}
		JNI
	}

	

	/**
	 * printing class and methodname + bytecode instructions + corresponding line number
	 */
	private def debug(classFile : ClassFile) : Unit = {
		/*
	   * For Source code purposes
	   */

		println(classFile.thisClass.className)

		for (method ← classFile.methods) {
			println(method.name)
			println(method)
			println(method.isAbstract)
			var line = 0
			for (instruction ← method.body.get.instructions if method.body.isDefined && !method.body.get.instructions.isEmpty) {

				if (instruction != null) println("\t" + line + " " + instruction)
				line += 1
			}
		}

		println
	}

	private def debugMethod(method : Method) : Unit = {
		/*
	   * For Source code purposes
	   */

		println(method.name)
		println(method.descriptor)

		var line = 0
		for (instruction ← method.body.get.instructions if !method.body.get.instructions.isEmpty) {

			if (instruction != null) println("\t" + line + " " + instruction)
			line += 1
		}

		println
	}

}