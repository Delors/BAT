package de.tud.cs.st.bat.resolved.analyses
import de.tud.cs.st.bat.resolved.reader.Java6Framework
import scala.io.Source
import java.io.File
import de.tud.cs.st.util.perf.PerformanceEvaluation
import de.tud.cs.st.util.perf.Counting

class Analyser
object Analyser extends Analyser {

	private val CountingPerformanceEvaluator = new PerformanceEvaluation with Counting
	import CountingPerformanceEvaluator._
	import de.tud.cs.st.util.perf._

	var stringBuilder = new StringBuilder()
	var fileName = ""
	def main(args : Array[String]) {

		if (args.length == 0 || !args.forall(arg ⇒ arg.endsWith(".zip") || arg.endsWith(".jar"))) {
			printUsage
			args.foreach(println)
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

	private def printUsage : Unit = {
		println("Usage: java …Main <ZIP or JAR file containing class files>+")
		println("(c) 2011 Michael Eichberg (eichberg@informatik.tu-darmstadt.de)")
	}

	def initStringBuilder() {
		stringBuilder = new StringBuilder()
		stringBuilder.append("ClassName;Exception Caught;Exception Thrown;Exposures;ReachableAssertion;Top-Level-Catch;Catching NullPointer;Logging;Debug Code Main;Debug Code Junit;Special Command;Path Traversal; Downloaded Code \n")
	}
	def init(zipFiles : Array[String]) {
		var classHierarchy = new ClassHierarchy

		for (zipFile ← zipFiles) {

			fileName = zipFile
			extractFileName

			time(t => println("Performing analysis for " + fileName + " took " + nsToSecs(t) + " secs")) {
				initStringBuilder
				var classFiles = for (classFile ← Java6Framework.ClassFiles(zipFile)) yield {

					classHierarchy = classHierarchy + classFile

					classFile
				}

				println("Classfiles: " + classFiles.length)

				for (classFile ← classFiles) {
					stringBuilder.append(classFile.thisClass.className + ";")
					analyse(classFile)
				}
				output()
			}
		}
	}

	def analyse(classFile : de.tud.cs.st.bat.resolved.reader.Java6Framework.ClassFile) = {
		exceptionAnalyses(classFile)
		inputAnalyses(classFile)
		stringBuilder.append("\n");

	}

	def output() {

		val folder = new File("output")
		if (!folder.exists()) {
			folder.mkdir()
		}
		val out = new java.io.FileWriter("Output\\" + fileName + ".csv")
		out.write(stringBuilder.toString())
		out.close
	}

	private def exceptionAnalyses(classFile : de.tud.cs.st.bat.resolved.reader.Java6Framework.ClassFile) : StringBuilder = {
		val exceptionAnalyser = ExceptionAnalyser

		stringBuilder.append(exceptionAnalyser.checkForOverlyBroadExceptionCatched(classFile).length + ";")
		stringBuilder.append(exceptionAnalyser.checkForOverlyBroadExceptionThrown(classFile).length + ";")
		stringBuilder.append(exceptionAnalyser.checkForExposureInErrorHandling(classFile).length + ";")
		stringBuilder.append(exceptionAnalyser.reachableAssertion(classFile).length + ";")
		stringBuilder.append(exceptionAnalyser.topLevelShouldCatchItAll(classFile).length + ";")
		stringBuilder.append(exceptionAnalyser.checkForCatchingNullPointer(classFile).length + ";")
		stringBuilder.append(exceptionAnalyser.ensureLogginDicipline(classFile).length + ";")
		stringBuilder.append(exceptionAnalyser.huntForDebugCodeMain(classFile).length + ";")
		stringBuilder.append(exceptionAnalyser.huntForDebugCodeJUnit(classFile).length + ";")
	}
	
	private def inputAnalyses(classFile : de.tud.cs.st.bat.resolved.reader.Java6Framework.ClassFile) : StringBuilder = {
		val inputAnalyser = InputAnalyser

		stringBuilder.append(inputAnalyser.specialElementsInCommand(classFile).length + ";")
		stringBuilder.append(inputAnalyser.pathTraversal(classFile).length + ";")
		stringBuilder.append(inputAnalyser.downloadedCode(classFile).length + ";")

	}

	private def extractFileName() : Unit = {
		val indexSlash = fileName.lastIndexOf("\\")
		val indexDot = fileName.lastIndexOf(".")
		fileName = fileName.substring(indexSlash + 1, indexDot)
	}
}