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
import de.tud.cs.st.bat.resolved.INVOKEVIRTUAL
import de.tud.cs.st.bat.resolved.GETSTATIC
import de.tud.cs.st.bat.resolved.ArrayType
import scala.collection.mutable.HashMap
import de.tud.cs.st.bat.resolved.INVOKESPECIAL

class ExceptionAnalyser
object ExceptionAnalyser extends ExceptionAnalyser {

//	private var hashMap = new HashMap[String, Int]()
//
//	private def initMap = {
//		hashMap.update("exTh", 0)
//		hashMap.update("exCa", 0)
//		hashMap.update("expose", 0)
//		hashMap.update("LD", 0)
//		hashMap.update("DcM", 0)
//		hashMap.update("DcJ", 0)
//	}

	private val debug = true;

//	private val CountingPerformanceEvaluator = new PerformanceEvaluation with Counting
//	import CountingPerformanceEvaluator._
//
//	import de.tud.cs.st.util.perf._
//
//	private def printUsage : Unit = {
//		println("Usage: java …Main <ZIP or JAR file containing class files>+")
//		println("(c) 2011 Michael Eichberg (eichberg@informatik.tu-darmstadt.de)")
//	}
//
//	def main(args : Array[String]) {
//
//		if (args.length == 0 || !args.forall(arg ⇒ arg.endsWith(".zip") || arg.endsWith(".jar"))) {
//			printUsage
//			sys.exit(1)
//		}
//
//		for (arg ← args) {
//			val file = new java.io.File(arg)
//			if (!file.canRead() || file.isDirectory()) {
//				println("The file: " + file + " cannot be read.");
//				printUsage
//				sys.exit(1)
//			}
//		}
//
//		init(args)
//
//		sys.exit(0)
//	}
//
//	def init(zipFiles : Array[String]) {
//		var classHierarchy = new ClassHierarchy
//
//		var classFilesCount = 0
//		val classFiles = time(t ⇒ println("Reading all class files took: " + nsToSecs(t))) {
//			for (zipFile ← zipFiles; classFile ← Java6Framework.ClassFiles(zipFile)) yield {
//				classFilesCount += 1
//				classHierarchy = classHierarchy + classFile
//				classFile
//			}
//		}
//		val exceptionInstructions = time(t ⇒ println("Exception Handling took: " + nsToSecs(t))) {
//			for (classFile ← classFiles) {
//				analyse(classFile)
//			}
//		}
//	}

//	def analyse(classFile : ClassFile) = {
//
//		hashMap.update("exTh", hashMap.get("exTh").get + checkForOverlyBroadExceptionThrown(classFile).length)
//		hashMap.update("exCa", hashMap.get("exCa").get + checkForOverlyBroadExceptionCatched(classFile).length)
//		hashMap.update("expose", hashMap.get("expose").get + checkForExposureInErrorHandling(classFile).length)
//		hashMap.update("LD", hashMap.get("LD").get + ensureLogginDicipline(classFile).length)
//		hashMap.update("DcM", hashMap.get("DcM").get + huntForDebugCodeMain(classFile).length)
//		hashMap.update("DcJ", hashMap.get("DcJ").get + huntForDebugCodeJUnit(classFile).length)
//	}

	def checkForOverlyBroadExceptionCatched(classFile : ClassFile) = {
		val exceptionType = ObjectType("java/lang/Exception")
		val trowable = ObjectType("java/lang/Throwable")
		var toGenerallExceptionThrown : List[(ClassFile, Method)] = Nil
		for (method ← classFile.methods if method.body.isDefined && !method.body.get.exceptionHandlers.isEmpty) {
			for (handler ← method.body.get.exceptionHandlers) {
				if (handler.catchType != null && (handler.catchType.equals(exceptionType) || handler.catchType.equals(trowable))) {
					toGenerallExceptionThrown = (classFile, method) :: toGenerallExceptionThrown
				}
			}
		}
		toGenerallExceptionThrown
	}

	def checkForOverlyBroadExceptionThrown(classFile : ClassFile) = {
		var toGenerallExceptionCatched : List[(ClassFile, Method)] = Nil
		val exceptionType = ObjectType("java/lang/Exception")
		val trowable = ObjectType("java/lang/Throwable")
		val error = ObjectType("java/lang/Error")
		val runtimeException = ObjectType("java/lang/RuntimeException")
		for (method ← classFile.methods; attribute ← method.attributes) {
			attribute match {
				case et : ExceptionTable => if (et.exceptions.contains(exceptionType) || et.exceptions.contains(trowable) || et.exceptions.contains(error) || et.exceptions.contains(runtimeException)) { toGenerallExceptionCatched = (classFile, method) :: toGenerallExceptionCatched }
				case _ =>
			}
		}
		toGenerallExceptionCatched
	}

	def checkForExposureInErrorHandling(classFile : ClassFile) = {
		var exposures : List[(ClassFile, Method)] = Nil
		val printStream = ObjectType("java/io/PrintStream")

		for (method ← classFile.methods; if !method.body.isEmpty && !method.body.get.instructions.isEmpty) {
			for (instruction ← method.body.get.instructions) {
				if (!method.body.get.exceptionHandlers.isEmpty) {
					instruction match {
						case iv : INVOKEVIRTUAL => if (iv.name.equals("printStackTrace")) exposures = (classFile, method) :: exposures
						case gs : GETSTATIC => if (gs.fieldType.equals(printStream)) exposures = (classFile, method) :: exposures
						case _ =>
					}
				}
			}
		}
		exposures
	}

	def reachableAssertion(classFile : ClassFile) = {
		var reachables : List[(ClassFile, Method)] = Nil
		val assertion = ObjectType("java/lang/AssertionError")

		for (method ← classFile.methods; if !method.body.isEmpty && !method.body.get.instructions.isEmpty) {
			for (instruction ← method.body.get.instructions) {
				instruction match {
					case iv : INVOKESPECIAL => if (iv.declaringClass.equals(assertion)) reachables = (classFile, method) :: reachables
					case _ =>
				}

			}
		}
		reachables
	}

	def topLevelShouldCatchItAll(classFile : ClassFile) = {
		var topLevelThrown : List[(ClassFile, Method)] = Nil
		for (method ← classFile.methods if method.body.isDefined && (method.name.equals("doGet") || method.name.equals("doPost"))) {
			method.attributes.foreach(att => att match {
				case et : ExceptionTable => topLevelThrown = (classFile, method) :: topLevelThrown
				case _ =>
			})
		}
		topLevelThrown

	}

	def checkForCatchingNullPointer(classFile : ClassFile) = {
		var nullPointerCatched : List[(ClassFile, Method)] = Nil
		val exceptionType = ObjectType("java/lang/NullPointerException")
		for (method ← classFile.methods; attribute ← method.attributes) {
			attribute match {
				case et : ExceptionTable => if (et.exceptions.contains(exceptionType)) nullPointerCatched = (classFile, method) :: nullPointerCatched
				case _ =>
			}
		}
		nullPointerCatched
	}

	def ensureLogginDicipline(classFile : ClassFile) = {
		var callsToSystemOutorErr : List[(ClassFile, Method)] = Nil
		val system = ObjectType("java/lang/System")

		for (method ← classFile.methods; if !method.body.isEmpty && !method.body.get.instructions.isEmpty) {
			for (instruction ← method.body.get.instructions) {
				instruction match {
					case gs : GETSTATIC =>
						if (gs.declaringClass.equals(system) && (gs.name.equals("err") || gs.name.equals("out"))) {
							callsToSystemOutorErr = (classFile, method) :: callsToSystemOutorErr
						}

					case _ =>
				}
			}
		}
		callsToSystemOutorErr
	}

	def huntForDebugCodeMain(classFile : ClassFile) = {
		var mainMethods : List[(ClassFile, Method)] = Nil
		val stringArray = ArrayType(ObjectType("java/lang/String"))

		for (method ← classFile.methods) {
			if (method.name == "main" && method.descriptor.parameterTypes.length == 1 && method.descriptor.parameterTypes.contains(stringArray)) {
				mainMethods = (classFile, method) :: mainMethods
			}
		}

		mainMethods
	}

	def huntForDebugCodeJUnit(classFile : ClassFile) = {
		var useOfJunit : List[(ClassFile, Method)] = Nil
		var classFlagAsTest = false
		val testcase = ObjectType("junit/framework/TestCase")
		classFile.superClass.foreach(superclass => if (superclass.equals(testcase)) {
			useOfJunit = (classFile, null) :: useOfJunit
			classFlagAsTest = true
		})
		if (!classFlagAsTest) {
			val junit = ObjectType("org/junit/Test")
			for (method ← classFile.methods; anno <- method.runtimeVisibleAnnotations) {
				anno.foreach(annot =>
					if (annot.annotationType.equals(junit)) useOfJunit = (classFile, method) :: useOfJunit)
			}
		}
		useOfJunit
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