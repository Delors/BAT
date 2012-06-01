package de.tud.cs.st.bat.resolved.analyses
import de.tud.cs.st.bat.resolved.reader.Java6Framework.ClassFile

class AnalysisObject(zipFile : String) {

	// authentication analyser
	var passwordUse : Int = 0
	var hardCodedCredentials : Int = 0

	// input analyser
	var specialCommandElements : Int = 0
	var pathTraversal : Int = 0
	var downloadedCode : Int = 0
	var SQLInjection : Int = 0
	var uncheckedRedirection : Int = 0

	// error analyser
	var exceptionsCaught : Int = 0
	var exceptionsThrown : Int = 0
	var exposures : Int = 0
	var reachableAssertion : Int = 0
	var topLevelCatch : Int = 0
	var catchNullPointer : Int = 0
	var loggingDicipline : Int = 0
	var debugCodeMainMethod : Int = 0
	var debugCodeJUnit : Int = 0

	//access analyser
	var arrays : Int = 0
	var fields : Int = 0
	var pFields : Int = 0
	var randoms : Int = 0
	var jnis : Int = 0

	//call analyser

	var cloneableNoClone : Int = 0
	var clonableWithoutSuperClone : Int = 0
	var cloneButNotCloneable : Int = 0
	var covariantCompareToMethods : Int = 0
	var garbageCollectingMethods : Int = 0
	var methodsThatCallRunFinalizersOnExit : Int = 0
	var abstractCovariantEquals : Int = 0
	var classesWithPublicFinalizeMethods : Int = 0
	var classesWithoutDefaultConstructor : Int = 0

	def csv() = {
		var csv = zipFile + "; ;"
		csv += exceptionsCaught + ";" + exceptionsThrown + ";" + exposures + ";" + reachableAssertion + ";" + topLevelCatch + ";" + catchNullPointer + ";" + loggingDicipline + ";" + debugCodeMainMethod + ";" + debugCodeMainMethod + ";"
		csv += specialCommandElements + ";" + pathTraversal + ";" + downloadedCode + ";" + SQLInjection + ";" + uncheckedRedirection + ";"
		csv += passwordUse + ";" + hardCodedCredentials + ";"
		csv += arrays + ";" + fields + ";" + pFields + ";" + randoms + ";" + jnis + ";"
		csv += cloneableNoClone+ ";" + clonableWithoutSuperClone+ ";" +cloneButNotCloneable + ";" + covariantCompareToMethods+ ";" + garbageCollectingMethods+ ";" +methodsThatCallRunFinalizersOnExit + ";" +abstractCovariantEquals+ ";" +classesWithPublicFinalizeMethods+ ";" +classesWithoutDefaultConstructor
		csv
	}
}