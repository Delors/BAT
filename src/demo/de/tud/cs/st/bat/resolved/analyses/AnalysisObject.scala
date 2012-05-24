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
	
	
	def csv() = {
		var csv = zipFile +";"
		csv += exceptionsCaught +";"+ exceptionsThrown +";"+ exposures  +";"+  reachableAssertion  +";"+ topLevelCatch  +";"+ catchNullPointer  +";"+ loggingDicipline  +";"+  debugCodeMainMethod  +";"+  debugCodeMainMethod +";"
		csv += specialCommandElements  +";"+ pathTraversal  +";"+ downloadedCode  +";"+  SQLInjection  +";"+  uncheckedRedirection +";"
		csv += passwordUse  +";"+ hardCodedCredentials
		csv
	}
}