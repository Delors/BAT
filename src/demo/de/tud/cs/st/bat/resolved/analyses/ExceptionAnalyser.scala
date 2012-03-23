package de.tud.cs.st.bat.resolved.analyses
import de.tud.cs.st.util.perf.PerformanceEvaluation
import de.tud.cs.st.util.perf.Counting
import de.tud.cs.st.bat.resolved.reader.Java6Framework
import sun.security.mscapi.PRNG
import de.tud.cs.st.bat.resolved.Method
import de.tud.cs.st.bat.resolved.ObjectType
import de.tud.cs.st.bat.resolved.ExceptionTable
import de.tud.cs.st.bat.resolved.ExceptionTable
import de.tud.cs.st.bat.resolved.ExceptionTable
import de.tud.cs.st.bat.resolved.ExceptionHandler
import de.tud.cs.st.bat.resolved.RETURN
import de.tud.cs.st.bat.resolved.ASTORE_2
import de.tud.cs.st.bat.resolved.ASTORE_1

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

		if (args.length == 0 || !args.forall(arg => arg.endsWith(".zip") || arg.endsWith(".jar"))) {
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

	def analyze(zipFiles : Array[String]) {
		var classHierarchy = new ClassHierarchy

		var classFilesCount = 0
		val classFiles = time(t => println("Reading all class files took: " + nsToSecs(t))) {
			for (zipFile ← zipFiles; classFile ← Java6Framework.ClassFiles(zipFile)) yield {
				classFilesCount += 1
				classHierarchy = classHierarchy + classFile
				classFile
			}
		}
		println("Classfiles: " + classFilesCount)

		var noErrorHandling : List[(String, String, Int)] = Nil
		var toGenerallExceptionThrown : List[(String, String)] = Nil
		var toGenerallExceptionCatched : List[(String, String)] = Nil
		val exceptionType = ObjectType("java/lang/Exception")

		val exceptionInstructions = time(t => println("Exception Handling took: " + nsToSecs(t))) {
			for (classFile ← classFiles) yield {

				println("Class : " + classFile.thisClass.className)

				val methods = for (method ← classFile.methods) yield (method)

				for (method ← methods) {

					println("Method : " + method.name);

					var catchTypes : List[ObjectType] = Nil;

					for (handler ← method.body.get.exceptionHandlers if method.body.isDefined && !method.body.get.exceptionHandlers.isEmpty) {

						println("ExceptionHandler: " + handler);

						catchTypes = handler.catchType :: catchTypes

						// checking for empty catch-statements
						var instructions = method.body.get.instructions
						var i = handler.handlerPC
						var end = false
						var error = false;
//						while (!end && i < instructions.size) {
//							//							println(end + " in while")
//							instructions.apply(i) match {
//								case RETURN =>
////									println(instructions(i) + " " + i);
//									i += 1
//									end = true;
//									if (error) {
//										noErrorHandling = (classFile.thisClass.className, method.name, handler.handlerPC) :: noErrorHandling
//									}
//								case ASTORE_1 | ASTORE_2 =>
////									println(instructions(i) + " " + i);
//									i += 1;
//									error = true;
//								case _ =>
////									println(instructions(i) + " " + i);
//									end = true;
//							}
//						}
					}
					// checking for throwing the broad exception "Exception"
					for (attribute ← method.attributes) {
						attribute match {
							case et : ExceptionTable => if (et.exceptions.contains(exceptionType)) { toGenerallExceptionThrown = (classFile.thisClass.className, method.name) :: toGenerallExceptionThrown }
							case _ =>
						}
					}

					// scanning for the overly broad exception "Exception" in a catch-statement
					if (catchTypes.contains(exceptionType)) {
						toGenerallExceptionCatched = (classFile.thisClass.className, method.name) :: toGenerallExceptionCatched
					}

					// DEBUG, see ByteCode Commands
					var line = 0
					for (instruction ← method.body.get.instructions if !method.body.get.instructions.isEmpty && !method.body.get.exceptionHandlers.isEmpty && debug) {
						println("\t" + line + " " + instruction)
						line += 1
					}

				}
				println;
				println("###################");
				println("###################");
				println;
			}
			println(noErrorHandling);
			println("Catch errors which are not handled: " + noErrorHandling.size)
			println("--------------------");
			println(toGenerallExceptionCatched);
			println("Catched to general exceptions " + toGenerallExceptionCatched.size)
			println("--------------------");
			println(toGenerallExceptionThrown);
			println("Thrown to general exceptions " + toGenerallExceptionThrown.size)
			println("--------------------");
		}
	}
}