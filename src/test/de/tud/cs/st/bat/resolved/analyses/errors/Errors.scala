/*  All rights reserved.
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
package de.tud.cs.st.bat.resolved.analyses.errors
import org.scalatest.FunSuite
import de.tud.cs.st.bat.resolved.reader.Java6Framework
import de.tud.cs.st.bat.resolved.analyses.ExceptionAnalyser

/**
 * @author Dennis Siebert
 */
@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class Errors extends FunSuite {

	private val classA = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exposure/Exposure.class")
	assert(classA ne null)

	private val classB = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exposure/DebugTest.class")
	assert(classB ne null)

	private val classC = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exposure/DebugSuperTest.class")
	assert(classC ne null)

	private val classD = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exposure/DebugMain.class")
	assert(classD ne null)

	private val classE = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exposure/Assert.class")
	assert(classE ne null)

	private val classF = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exceptions/TopLevel.class")
	assert(classF ne null)

	private val classG = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exceptions/TopLevelCatch.class")
	assert(classG ne null)

	private val classH = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exceptions/NullPointer.class")
	assert(classH ne null)

	//	private val classAMQ = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exposure/AMQJournalTool.class")
	//	assert(classAMQ ne null)

	private val classI = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exceptions/TooGeneralException.class")
	assert(classI ne null)

	private val classJ = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exceptions/HandledException.class")
	assert(classJ ne null)

	private val classK = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exceptions/TooGeneralException.class")
	assert(classK ne null)

	private val classL = Java6Framework.ClassFile("test/classfiles/Errors.zip", "errors/exceptions/HandledException.class")
	assert(classL ne null)

	private val exceptionAnalyser = ExceptionAnalyser

	test("Exposure in exception handling") {

		val result = exceptionAnalyser.checkForExposureInErrorHandling(classA)
		println(result.size)
		assert(result.size == 3)
	}

	//	test("Exposure in exception handling classAMQ") {
	//
	//		val result = exceptionAnalyser.checkForExposureInErrorHandling(classAMQ)
	//		println(result.size)
	//		
	//	}

	test("Hunt for System out and err") {

		val result = exceptionAnalyser.ensureLogginDicipline(classA)
		assert(result.size == 4)
	}

	test("Hunt for Unit Tests") {

		val result = exceptionAnalyser.huntForDebugCodeJUnit(classB)
		assert(result.size == 1)
	}

	test("Hunt for Unit Testcase") {

		val result = exceptionAnalyser.huntForDebugCodeJUnit(classC)
		assert(result.size == 1)
	}

	test("Hunt for Main Method") {

		val result = exceptionAnalyser.huntForDebugCodeMain(classD)
		assert(result.size == 1)
	}

	test("Reachable Assert") {

		val result = exceptionAnalyser.reachableAssertion(classE)
		assert(result.size == 2)
	}

	test("Top Level Thrown") {

		val result = exceptionAnalyser.topLevelShouldCatchItAll(classF)
		assert(result.size == 2)
	}

	test("Top Level Catch") {

		val result = exceptionAnalyser.topLevelShouldCatchItAll(classG)
		assert(result.size == 0)
	}

	test("NullPointer") {

		val result = exceptionAnalyser.checkForCatchingNullPointer(classH)
		assert(result.size == 1)
	}

	test("Excecption catched to general") {

		val result = exceptionAnalyser.checkForOverlyBroadExceptionCatched(classI)
		assert(result.size == 1)
	}

	test("Exception catched spezialized") {

		val result = exceptionAnalyser.checkForOverlyBroadExceptionCatched(classJ)
		assert(result.size == 0)
	}

	test("Excecption thrown to General") {

		val result = exceptionAnalyser.checkForOverlyBroadExceptionThrown(classK)
		assert(result.size == 1)
	}

	test("Exception thrown spezialized") {

		val result = exceptionAnalyser.checkForOverlyBroadExceptionThrown(classL)
		assert(result.size == 0)
	}

}