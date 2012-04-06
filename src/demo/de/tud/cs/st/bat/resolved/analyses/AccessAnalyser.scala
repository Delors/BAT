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

		analyze(args)

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

	def analyze(zipFiles : Array[String]) {
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

				var psNotFinal = FieldNotFinal(classFile)
				var r = ArrayPSF(classFile);
				printResults(psNotFinal, r)
			}
		}

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
	 * checks for array fields, which public static final. The array may be immutable but not its content.
	 */
	def ArrayPSF(classFile : ClassFile) = {
		for (
			field ← classFile.fields if field.fieldType.isArrayType &&
				field.isPublic && field.isStatic && field.isFinal
		) yield (classFile, field)
	}

	/**
	 * prints the current results of both analysis
	 */
	private def printResults(psNotFinal : IndexedSeq[(ClassFile, de.tud.cs.st.bat.resolved.Field)], r : IndexedSeq[(ClassFile, de.tud.cs.st.bat.resolved.Field)]) : Unit = {

		println

		println("Found public static fields without final modifier: " + psNotFinal.size)
		println("--------------------");

		println("Found public static final arrays " + r.size)
		println("--------------------");
	}

}