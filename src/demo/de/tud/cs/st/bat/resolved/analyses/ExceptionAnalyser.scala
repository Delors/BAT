package de.tud.cs.st.bat.resolved.analyses
import de.tud.cs.st.bat.resolved.reader.Java6Framework
import de.tud.cs.st.bat.resolved.ExceptionHandler
import de.tud.cs.st.bat.resolved.ExceptionTable
import de.tud.cs.st.bat.resolved.ExceptionTable
import de.tud.cs.st.bat.resolved.ExceptionTable
import de.tud.cs.st.bat.resolved.Method
import de.tud.cs.st.bat.resolved.ObjectType
import de.tud.cs.st.util.perf.Counting
import de.tud.cs.st.util.perf.PerformanceEvaluation
import de.tud.cs.st.bat.resolved.reader.Java6Framework.ClassFile
import de.tud.cs.st.bat.resolved.reader.Java6Framework

class ExceptionAnalyser
object ExceptionAnalyser extends ExceptionAnalyser {

	private val debug = true;

	private val CountingPerformanceEvaluator = new PerformanceEvaluation with Counting
	import CountingPerformanceEvaluator._

	import de.tud.cs.st.util.perf._

	private def printUsage : Unit = {
		println("Usage: java …Main <ZIP or JAR file containing class files>+")
		println("(c) 2011 Michael Eichberg (eichberg@informatik.tu-darmstadt.de)")
	}

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
		val exceptionInstructions = time(t ⇒ println("Exception Handling took: " + nsToSecs(t))) {
			for (classFile ← classFiles) {
				analyse(classFile)
			}
		}
	}

	def analyse(classFile : ClassFile) = {
		checkForOverlyBroadExceptionThrown(classFile)
		checkForOverlyBroadExceptionsCatched(classFile)
	}

	def checkForOverlyBroadExceptionThrown(classFile : ClassFile) = {
		val exceptionType = ObjectType("java/lang/Exception")
		var toGenerallExceptionThrown : List[(ClassFile, Method)] = Nil
		for (method ← classFile.methods if method.body.isDefined && !method.body.get.exceptionHandlers.isEmpty; handler ← method.body.get.exceptionHandlers) {
			if (handler.catchType.equals(exceptionType)) {
				toGenerallExceptionThrown = (classFile, method) :: toGenerallExceptionThrown
			}
		}
		toGenerallExceptionThrown
	}

	def checkForOverlyBroadExceptionsCatched(classFile : ClassFile) = {
		var toGenerallExceptionCatched : List[(ClassFile, Method)] = Nil
		val exceptionType = ObjectType("java/lang/Exception")
		for (method ← classFile.methods ; attribute ← method.attributes) {
			attribute match {
				case et : ExceptionTable ⇒ if (et.exceptions.contains(exceptionType)) { toGenerallExceptionCatched = (classFile, method) :: toGenerallExceptionCatched }
				case _ ⇒
			}
		}
		toGenerallExceptionCatched
	}
}