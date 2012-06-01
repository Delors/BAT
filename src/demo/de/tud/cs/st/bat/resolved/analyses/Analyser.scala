package de.tud.cs.st.bat.resolved.analyses
import de.tud.cs.st.bat.resolved.reader.Java6Framework
import de.tud.cs.st.bat.resolved.reader.Java6Framework.ClassFile
import scala.io.Source
import java.io.File
import de.tud.cs.st.util.perf.PerformanceEvaluation
import de.tud.cs.st.util.perf.Counting
import de.tud.cs.st.bat.resolved.ObjectType
import scala.collection.mutable.Map

class Analyser{
	var classHierarchy = new ClassHierarchy
	var classFiles : Seq[(ClassFile)] = Nil
} 

object Analyser extends Analyser{	
	

	private val CountingPerformanceEvaluator = new PerformanceEvaluation with Counting
	import CountingPerformanceEvaluator._
	import de.tud.cs.st.util.perf._

	var stringBuilder = new StringBuilder()
	var overallStringBuilder = new StringBuilder()

	var fileName = ""
	def main(args : Array[String]) {

		//		if (args.length == 0 || !args.forall(arg ⇒ arg.endsWith(".zip") || arg.endsWith(".jar"))) {
		//			printUsage
		//			args.foreach(println)
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
		//		init(args)

		time(t => println("The whole analysing took " + nsToSecs(t) + " secs")) {

			overallStringBuilder = initStringBuilder()
			iterateThoughFolders(new File("test-applications"))
			//			println(overallStringBuilder.toString())
			output("overallAnalysis", overallStringBuilder)
		}
		sys.exit(0)

	}
	def iterateThoughFolders(start : File) {

		for (file <- start.listFiles() if !start.listFiles().isEmpty) {

			if (file.getName().endsWith(".jar") || file.getName().endsWith(".war")) {
				println("processing " + file)
				val analysis = initForSingle(file.getAbsolutePath())
				overallStringBuilder.append(analysis.csv())
				overallStringBuilder.append("\n")
				println
			} else {
				iterateThoughFolders(file)
			}
		}

	}

	private def printUsage : Unit = {
		println("Usage: java …Main <ZIP or JAR file containing class files>+")
		println("(c) 2011 Michael Eichberg (eichberg@informatik.tu-darmstadt.de)")
	}

	def initStringBuilder() = {
		stringBuilder = new StringBuilder()
		stringBuilder.append("Name;Link;Exception Caught;Exception Thrown;Exposures;ReachableAssertion;Top-Level-Catch;Catching NullPointer;Logging;Debug Code Main;Debug Code Junit")
		stringBuilder.append(";Special Command;Path Traversal; Downloaded Code; SQL Injection;Unchecked Redirection")
		stringBuilder.append(";Passwords;Hard-coded SQL Credentials")
		stringBuilder.append(";Arrays;Fields;Protected Fields;PRNG;JNI")
		stringBuilder.append(";cloneableNoClone;clonableWithoutSuperClone;cloneButNotCloneable;covariantCompareToMethods;garbageCollectingMethods;methodsThatCallRunFinalizersOnExit;abstractCovariantEquals;classesWithPublicFinalizeMethods;classesWithoutDefaultConstructor")
		stringBuilder.append("\n")
		stringBuilder
	}

	def initForJar(zipFiles : Array[String]) {

		for (zipFile ← zipFiles) {

			fileName = zipFile
			extractFileName

			time(t => println("Performing analysis for " + fileName + " took " + nsToSecs(t) + " secs")) {

				classFiles = for (classFile ← Java6Framework.ClassFiles(zipFile)) yield {

					classHierarchy = classHierarchy + classFile

					classFile
				}
				stringBuilder = initStringBuilder
				println("Classfiles: " + classFiles.length)

				for (classFile ← classFiles) {
					stringBuilder.append(classFile.thisClass.className + ";")
					//					analyse(classFile)
				}
				output(fileName, stringBuilder)
			}
		}
	}

	def initForSingle(zipFile : String) = {

		fileName = zipFile
		extractFileName

		val analysis = new AnalysisObject(fileName)

		time(t => println("Performing analysis for " + fileName + " took " + nsToSecs(t) + " secs")) {

			classFiles = for (classFile ← Java6Framework.ClassFiles(zipFile)) yield {
				classHierarchy = classHierarchy + classFile
				classFile
			}

			println("Classfiles: " + classFiles.length)
			stringBuilder = initStringBuilder

			for (classFile ← classFiles) {
				stringBuilder.append(classFile.thisClass.className + ";" + "=HYPERLINK(\"http://grepcode.com/search?query=" + classFile.thisClass.className + "&start=0&entity=type&n=)\");")
				analyse(classFile, analysis)
			}
			output(fileName, stringBuilder)
		}
		analysis
	}

	def analyse(classFile : ClassFile, analysis : AnalysisObject) = {
		exceptionAnalyses(classFile, analysis)
		inputAnalyses(classFile, analysis)
		authAnalyses(classFile, analysis)
		accessAnalyses(classFile, analysis)
		callAnalyses(classFile, analysis)
		stringBuilder.append("\n");

	}

	def output(filename : String, stringBuilder : StringBuilder) {

		val folder = new File("output")
		if (!folder.exists()) {
			folder.mkdir()
		}
		val out = new java.io.FileWriter("Output\\" + filename + ".csv")
		out.write(stringBuilder.toString())
		out.close
	}

	private def exceptionAnalyses(classFile : ClassFile, analysis : AnalysisObject) = {
		val exceptionAnalyser = ExceptionAnalyser
		val exCA = exceptionAnalyser.checkForOverlyBroadExceptionThrown(classFile).length
		val exTh = exceptionAnalyser.checkForOverlyBroadExceptionCatched(classFile).length
		val expo = exceptionAnalyser.checkForExposureInErrorHandling(classFile).length
		val reachA = exceptionAnalyser.reachableAssertion(classFile).length
		val topL = exceptionAnalyser.topLevelShouldCatchItAll(classFile).length
		val cNull = exceptionAnalyser.checkForCatchingNullPointer(classFile).length
		val log = exceptionAnalyser.ensureLogginDicipline(classFile).length
		val debugM = exceptionAnalyser.huntForDebugCodeMain(classFile).length
		val debugJ = exceptionAnalyser.huntForDebugCodeJUnit(classFile).length

		analysis.exceptionsCaught += exCA
		analysis.exceptionsThrown += exTh
		analysis.exposures += expo
		analysis.reachableAssertion += reachA
		analysis.topLevelCatch += topL
		analysis.catchNullPointer += cNull
		analysis.loggingDicipline += log
		analysis.debugCodeMainMethod += debugM
		analysis.debugCodeJUnit += debugJ

		stringBuilder.append(exCA + ";")
		stringBuilder.append(exTh + ";")
		stringBuilder.append(expo + ";")
		stringBuilder.append(reachA + ";")
		stringBuilder.append(topL + ";")
		stringBuilder.append(cNull + ";")
		stringBuilder.append(log + ";")
		stringBuilder.append(debugM + ";")
		stringBuilder.append(debugJ + ";")
	}

	private def inputAnalyses(classFile : ClassFile, analysis : AnalysisObject) = {
		val inputAnalyser = InputAnalyser
		val special = inputAnalyser.specialElementsInCommand(classFile).length
		val path = inputAnalyser.pathTraversal(classFile).length
		val download = inputAnalyser.downloadedCode(classFile).length
		val sql = inputAnalyser.SQLInjection(classFile).length
		val redirect = inputAnalyser.uncheckedRedirection(classFile).length

		analysis.specialCommandElements += special
		analysis.pathTraversal += path
		analysis.downloadedCode += download
		analysis.SQLInjection += sql
		analysis.uncheckedRedirection += redirect

		stringBuilder.append(special + ";")
		stringBuilder.append(path + ";")
		stringBuilder.append(download + ";")
		stringBuilder.append(sql + ";")
		stringBuilder.append(redirect + ";")
	}

	private def authAnalyses(classFile : ClassFile, analysis : AnalysisObject) = {
		val authAnalyser = AuthAnalyser
		val passwords = authAnalyser.checkForPasswords(classFile).length
		val hardCoded = authAnalyser.hardCodedSQLCredentials(classFile).length

		analysis.passwordUse += passwords
		analysis.hardCodedCredentials += hardCoded

		stringBuilder.append(passwords + ";")
		stringBuilder.append(hardCoded + ";")
	}

	private def accessAnalyses(classFile : ClassFile, analysis : AnalysisObject) = {
		val accessAnalyser = AccessAnalyser

		val arrays = accessAnalyser.ArrayPSF(classFile).length
		val fields = accessAnalyser.FieldNotFinal(classFile).length
		val pFields = accessAnalyser.protectedFields(classFile).length
		val randoms = accessAnalyser.RandomSeedAnalyser(classFile).length
		val jnis = accessAnalyser.unsafeUseOfJNI(classFile).length

		analysis.arrays += arrays
		analysis.fields += fields
		analysis.pFields += pFields
		analysis.randoms += randoms
		analysis.jnis += jnis

		stringBuilder.append(arrays + ";")
		stringBuilder.append(fields + ";")
		stringBuilder.append(pFields + ";")
		stringBuilder.append(randoms + ";")
		stringBuilder.append(jnis + ";")
	}

	private def callAnalyses(classFile : ClassFile, analysis : AnalysisObject) = {
		val callAnalyser = CallAnalyser

		var cloneableNoClone = callAnalyser.cloneableNoClone(classFile).length
		var clonableWithoutSuperClone = callAnalyser.clonableWithoutSuperClone(classFile).length
		var cloneButNotCloneable = callAnalyser.cloneButNotCloneable(classFile).length
		var covariantCompareToMethods = callAnalyser.covariantCompareToMethods(classFile).length
		var garbageCollectingMethods = callAnalyser.garbageCollectingMethods(classFile).length
		var methodsThatCallRunFinalizersOnExit = callAnalyser.methodsThatCallRunFinalizersOnExit(classFile).length
		var abstractCovariantEquals = callAnalyser.abstractCovariantEquals(classFile).length
		var classesWithPublicFinalizeMethods = callAnalyser.classesWithPublicFinalizeMethods(classFile).length
		var classesWithoutDefaultConstructor = callAnalyser.classesWithoutDefaultConstructor(classFile).size

		analysis.cloneableNoClone = cloneableNoClone
		analysis.clonableWithoutSuperClone = clonableWithoutSuperClone
		analysis.cloneButNotCloneable = cloneButNotCloneable
		analysis.covariantCompareToMethods = covariantCompareToMethods
		analysis.garbageCollectingMethods = garbageCollectingMethods
		analysis.methodsThatCallRunFinalizersOnExit = methodsThatCallRunFinalizersOnExit
		analysis.abstractCovariantEquals = abstractCovariantEquals
		analysis.classesWithPublicFinalizeMethods = classesWithPublicFinalizeMethods
		analysis.classesWithoutDefaultConstructor = classesWithoutDefaultConstructor

		stringBuilder.append(cloneableNoClone + ";")
		stringBuilder.append(clonableWithoutSuperClone + ";")
		stringBuilder.append(cloneButNotCloneable + ";")
		stringBuilder.append(covariantCompareToMethods + ";")
		stringBuilder.append(garbageCollectingMethods + ";")
		stringBuilder.append(methodsThatCallRunFinalizersOnExit + ";")
		stringBuilder.append(abstractCovariantEquals + ";")
		stringBuilder.append(classesWithPublicFinalizeMethods + ";")
		stringBuilder.append(classesWithoutDefaultConstructor + ";")
	}

	private def extractFileName() : Unit = {
		val indexSlash = fileName.lastIndexOf("\\")
		val indexDot = fileName.lastIndexOf(".")
		fileName = fileName.substring(indexSlash + 1, indexDot)
	}
}