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
class CallAnalyser extends Analyser 
object CallAnalyser extends CallAnalyser{


	def cloneableNoClone(classFile : ClassFile) = {

		var cloneMethod : List[(ClassFile, Method)] = Nil
		if (classFile.interfaces.contains(ObjectType("java/lang/Cloneable")) && !classFile.methods.exists({
			case Method(_, "clone", MethodDescriptor(Seq(), ObjectType.Object), _) ⇒ true;
			case _ ⇒ false;
		})) {
			cloneMethod = (classFile, null) :: cloneMethod
		}

		cloneMethod
	}

	def clonableWithoutSuperClone(classFile : ClassFile) = {
		var noSuperClone : List[(ClassFile, Method)] = Nil
		if (!classFile.isInterfaceDeclaration && !classFile.isAnnotationDeclaration) {
			for {
				superClass <- classFile.superClass.toList

				method @ Method(_, "clone", MethodDescriptor(Seq(), ObjectType.Object), _) ← classFile.methods

				if (!method.isAbstract && !method.body.get.instructions.exists({
					case INVOKESPECIAL(`superClass`, "clone", MethodDescriptor(Seq(), ObjectType.Object)) ⇒ true;
					case _ ⇒ false;
				}))
			} {
				noSuperClone = (classFile, null) :: noSuperClone
			}
		}
		noSuperClone
	}

	def cloneButNotCloneable(classFile : ClassFile) = {
		var notCloneable : List[(ClassFile, Method)] = Nil

		if (!classFile.isAnnotationDeclaration && classFile.superClass.isDefined) {
			for {

				method @ Method(_, "clone", MethodDescriptor(Seq(), ObjectType.Object), _) ← classFile.methods
				if !classHierarchy.isSubtypeOf(classFile.thisClass, ObjectType("java/lang/Cloneable")).getOrElse(false)
			} {
				notCloneable = (classFile, null) :: notCloneable
			}
		}
		notCloneable
	}

	def covariantCompareToMethods(classFile : ClassFile) = {
		var covariantCompareToMethods : List[(ClassFile, Method)] = Nil

		if (classFile.interfaces.contains(ObjectType("java/lang/Comparable"))) {
			for {
				method @ Method(_, "compareTo", MethodDescriptor(Seq(parameterType), IntegerType), _) ← classFile.methods
				if parameterType != ObjectType("java/lang/Object")
			} {
				covariantCompareToMethods = (classFile, null) :: covariantCompareToMethods
			}
		}
		covariantCompareToMethods
	}

	def garbageCollectingMethods(classFile : ClassFile) = {
		var garbageCollecting : List[(ClassFile, Method)] = Nil

		if (!classFile.thisClass.className.startsWith("java/lang")) {
			for ( // we don't care about gc calls in java.lang and also about gc calls that happen inside of methods related to garbage collection (heuristic)

				method ← classFile.methods if method.body.isDefined && !"(^gc)|(gc$)".r.findFirstIn(method.name).isDefined;
				instruction ← method.body.get.instructions
			) {
				instruction match {
					case INVOKESTATIC(ObjectType("java/lang/System"), "gc", MethodDescriptor(Seq(), VoidType)) |
						INVOKEVIRTUAL(ObjectType("java/lang/Runtime"), "gc", MethodDescriptor(Seq(), VoidType)) ⇒
						garbageCollecting = (classFile, method) :: garbageCollecting
					case _ ⇒
				}
			}
		}
		garbageCollecting
	}

	def methodsThatCallRunFinalizersOnExit(classFile : ClassFile) = {

		var methodsThatCallRunFinalizersOnExit : List[(ClassFile, Method)] = Nil

		for (
			method ← classFile.methods if method.body.isDefined;
			instruction ← method.body.get.instructions
		) {
			instruction match {
				case INVOKESTATIC(ObjectType("java/lang/System"), "runFinalizersOnExit", MethodDescriptor(Seq(BooleanType), VoidType)) |
					INVOKESTATIC(ObjectType("java/lang/Runtime"), "runFinalizersOnExit", MethodDescriptor(Seq(BooleanType), VoidType)) ⇒
					methodsThatCallRunFinalizersOnExit = (classFile, method) :: methodsThatCallRunFinalizersOnExit
				case _ ⇒
			}
		}
		methodsThatCallRunFinalizersOnExit
	}

	def abstractCovariantEquals(classFile : ClassFile) = {
		for (
			method @ Method(_, "equals", MethodDescriptor(Seq(classFile.thisClass), BooleanType), _) ← classFile.methods if method.isAbstract
		) yield (classFile, method);
	}

	def classesWithPublicFinalizeMethods(classFile : ClassFile) = {
		var finalizerMethods : List[(ClassFile, Method)] = Nil
		if (classFile.methods.exists(method =>
			method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0)) {
			finalizerMethods = (classFile, null) :: finalizerMethods
		}
		finalizerMethods
	}

	def classesWithoutDefaultConstructor(classFile : ClassFile) = {
		val serializableClasses = classHierarchy.subclasses(ObjectType("java/io/Serializable")).getOrElse(Set.empty)
		val getClassFile = classFiles.map(cf ⇒ (cf.thisClass, cf)).toMap
		for (
			superclass ← classHierarchy.superclasses(serializableClasses) if getClassFile.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
				{
					val superClassFile = getClassFile(superclass)
					!superClassFile.isInterfaceDeclaration &&
					!superClassFile.constructors.exists(_.descriptor.parameterTypes.length == 0)
				}
		) yield (superclass,null) // there can be at most one method
	}

	//	/**
	//	 * checks if a if or while statement contains a fixed boolean expression and therefore is always true or false
	//	 * NOT IMPLEMENTED YET
	//	 */
	//	def FixedExpresson(classFile : ClassFile) = {
	//		debug(classFile)
	//	}
	//
	//	/**
	//	 * checks for just comparing the object reference then the actual object content instead
	//	 * NOT IMPLEMENTED YET
	//	 */
	//	def CompareobjectReference(classFile : ClassFile) = {
	//		debug(classFile)
	//	}
	//
	//		/**
	//	 * checks for assigning inside if statement instead of checking boolean value or compare for a boolean value
	//	 * NOT IMPLEMENTED YET
	//	 */
	//	
	//	def AssigningInsteadCompare(classFile : ClassFile) = {
	//	
	//	}

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