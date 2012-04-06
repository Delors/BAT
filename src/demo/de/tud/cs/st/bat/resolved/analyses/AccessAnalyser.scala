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


/**
 *
 * @author Dennis Siebert
 */
class AccessAnalyser
object AccessAnalyser extends AccessAnalyser {
	def main(args : Array[String]) {

		if (args.length == 0 || !args.forall(arg ⇒ arg.endsWith(".zip") || arg.endsWith(".jar"))) {
			printUsage
			sys.exit(1)
		}

		for (arg ← args) {
			val file = new java.io.File(arg)
			if (!file.canRead() || file.isDirectory()) {
				println("The file: " + file + " cannot be read.");
				printUsage
				sys.exit(1)
			}
		}

		init(args)

		sys.exit(0)
	}
	private val debug = true;

	private val CountingPerformanceEvaluator = new PerformanceEvaluation with Counting
	import CountingPerformanceEvaluator._
	import de.tud.cs.st.util.perf._

	private def printUsage : Unit = {
		println("Usage: java …Main <ZIP or JAR file containing class files>+")
		println("(c) 2011 Michael Eichberg (eichberg@informatik.tu-darmstadt.de)")
	}

	def init(zipFiles : Array[String]) {
		var classHierarchy = new ClassHierarchy

		var classFilesCount = 0
		val classFiles = time(t ⇒ println("Reading all class files took: " + nsToSecs(t))) {
			for (zipFile ← zipFiles; classFile ← Java6Framework.ClassFiles(zipFile)) yield {
				classFilesCount += 1
				classHierarchy = classHierarchy + classFile
				classFile
			}
		}
		println("Classfiles: " + classFilesCount)

		val accessInstructions = time(t ⇒ println("Access Handling " + nsToSecs(t))) {
			for (classFile ← classFiles) {
				analyse(classFile)
			}
		}
	}

	private def analyse(classFile : de.tud.cs.st.bat.resolved.reader.Java6Framework.ClassFile) : Unit = {
		var psNotFinal = FieldNotFinal(classFile)
		var Arrays = ArrayPSF(classFile);
		var assignings = AssigningInsteadCompare(classFile)
		printResults(psNotFinal.size, Arrays.size, assignings.size)
	}

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
	 * checks for assigning inside if statement instead of checking boolean value or compare for a boolean value
	 */
	def AssigningInsteadCompare(classFile : ClassFile) = {
		var wrongIfStatement : List[(ClassFile, Method)] = Nil
		for (method ← classFile.methods) {
			var instructions = method.body.get.instructions
			var end = false
			var assigning = false;
			var i = 0
			while (!end && i < instructions.size) {

				instructions.apply(i) match {
					case ISTORE_0 | ISTORE_1 | ISTORE_2 | ISTORE_3 | ISTORE(_) ⇒
						assigning = true;
						i += 1
					case IFEQ(_) ⇒
						if (assigning) {
							end = true;
							wrongIfStatement = (classFile, method) :: wrongIfStatement
						}
						assigning = false
						i += 1
					case _ ⇒
						assigning = false
						i += 1
				}
			}
		}
		wrongIfStatement
	}

	/**
	 * checks if a if or while statement contains a fixed boolean expression and therefore is always true or false
	 * NOT IMPLEMENTED YET
	 */
	def RandomSeedAnalyser(classFile : ClassFile) = {
		var predictableSeed : List[(ClassFile, Method)] = Nil
		val randomType = ObjectType("java/util/Random")
		for (method ← classFile.methods if !method.body.get.instructions.isEmpty) {
			var instructions = method.body.get.instructions
			var end = false
			var assigning = false;
			var i = 0
			var lastInstruction = instructions.apply(i)
			while (!end && i < instructions.size) {
				instructions.apply(i) match {
					case INVOKESPECIAL(randomType, _, _) ⇒
						lastInstruction match {
							case LDC2_W(ConstantLong(_)) | LDC2_W(ConstantInteger(_)) ⇒
							predictableSeed = (classFile,method) :: predictableSeed
							case _ ⇒

						}
						i += 1
					case _ ⇒
						if (instructions.apply(i) != null) {
							lastInstruction = instructions.apply(i)
						}
						i += 1
				}
			}
		}
		predictableSeed
	}

	/**
	 * checks if a if or while statement contains a fixed boolean expression and therefore is always true or false
	 * NOT IMPLEMENTED YET
	 */
	def FixedExpresson(classFile : ClassFile) = {
		debug(classFile)
	}

	/**
	 * checks for just comparing the object reference then the actual object content instead
	 * NOT IMPLEMENTED YET
	 */
	def ComparbjectReference(classFile : ClassFile) = {
		debug(classFile)
	}

	/**
	 * prints the current results of both analysis
	 */
	private def printResults(psNotFinalSize : Integer, ArraysSize : Integer, AssignSize : Integer) : Unit = {

		println

		println("Found public static fields without final modifier: " + psNotFinalSize)
		println("--------------------");

		println("Found public static final arrays " + ArraysSize)
		println("--------------------");

		println("Found wrong if assignings " + AssignSize)
		println("--------------------");
	}

	/**
	 * printing class and methodname + bytecode instructions + corresponding line number
	 */
	private def debug(classFile : de.tud.cs.st.bat.resolved.reader.Java6Framework.ClassFile) : Unit = {
		/*
	   * For Source code purposes
	   */
		var line = 0
		println(classFile.thisClass.className)

		for (method ← classFile.methods) {
			println(method.name)
		}
		for (method ← classFile.methods; instruction ← method.body.get.instructions if !method.body.get.instructions.isEmpty) {
			if (instruction != null) println("\t" + line + " " + instruction)
			line += 1
		}
		println
	}

}