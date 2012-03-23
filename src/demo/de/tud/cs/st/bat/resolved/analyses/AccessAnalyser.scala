package de.tud.cs.st.bat.resolved.analyses

import de.tud.cs.st.bat.resolved.reader.Java6Framework
import de.tud.cs.st.util.perf.PerformanceEvaluation
import de.tud.cs.st.util.perf.Counting
import de.tud.cs.st.bat.resolved.ObjectType
import de.tud.cs.st.bat.resolved.ArrayType
import de.tud.cs.st.bat.resolved.Field

class AccessAnalyser
object AccessAnalyser extends AccessAnalyser {

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

		val accessInstructions = time(t => println("Access Handling " + nsToSecs(t))) {

				var psNotFinal = for (classFile ← classFiles;
					field <- classFile.fields if field.isPublic && field.isStatic && !field.isFinal) yield (classFile,field);

				var r = for(classFile <- classFiles;
				field <- classFile.fields if field.fieldType.isArrayType && 
					field.isPublic && field.isStatic && field.isFinal) yield (classFile,field);

				

		
			
/*			for (classFile ← classFiles) yield {

				for (field <- classFile.fields) {
					if (field.isPublic && field.isStatic && !field.isFinal) {
						psButNotFinal = (classFile.thisClass.className, field.name) :: psButNotFinal
					}
				}

				for (field <- classFile.fields) {
					field.fieldType match {
						case x : ArrayType => if (field.isPublic && field.isStatic && field.isFinal) arrayAsPST = (classFile.thisClass.className, field.name) :: arrayAsPST
						case _ =>
					}
				}

			}*/
			println
		
			println("Found public static fields without final modifier: " + psNotFinal.size)
			println("--------------------");
			
			println("Found public static final arrays " + r.size)
			println("--------------------");
		}

	}

}