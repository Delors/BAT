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
package de.tud.cs.st.bat.resolved.analyses.calls
import org.scalatest.FunSuite
import de.tud.cs.st.bat.resolved.reader.Java6Framework
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import de.tud.cs.st.bat.resolved.analyses.ClassHierarchy
import de.tud.cs.st.bat.resolved.analyses.CallAnalyser

/**
 * @author Dennis Siebert
 */
@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class CallsTest extends FlatSpec with ShouldMatchers { 
private val classH = Java6Framework.ClassFile("test/classfiles/Calls.zip", "calls/CloneableNoClone.class")
	assert(classH ne null)

	private val classI = Java6Framework.ClassFile("test/classfiles/Calls.zip", "calls/CloneNoSuperCloneCall.class")
	assert(classI ne null)

	private val classJ = Java6Framework.ClassFile("test/classfiles/Calls.zip", "calls/SubCloneableNoClone.class")
	assert(classJ ne null)

	private val classK = Java6Framework.ClassFile("test/classfiles/Calls.zip", "calls/CovariantComparable.class")
	assert(classK ne null)

	private val classL = Java6Framework.ClassFile("test/classfiles/Calls.zip", "calls/CallToGc.class")
	assert(classL ne null)

	private val classM = Java6Framework.ClassFile("test/classfiles/Calls.zip", "calls/RunFinalizer.class")
	assert(classM ne null)

	private val classN = Java6Framework.ClassFile("test/classfiles/Calls.zip", "calls/AbstractCovariantEquals.class")
	assert(classN ne null)

	private val classO = Java6Framework.ClassFile("test/classfiles/Calls.zip", "calls/Finalizer.class")
	assert(classO ne null)
	
		private val classP = Java6Framework.ClassFile("test/classfiles/Calls.zip", "calls/A.class")
	assert(classP ne null)
		

	var classHierarchy = new ClassHierarchy

	var classFilesCount = 0
	val classFiles = {
		for (
			classFile ← Java6Framework.ClassFiles("test/classfiles/Calls.zip") if (classFile ne null)
		) yield {
			classFilesCount += 1
			classHierarchy = classHierarchy + classFile
			classFile
		}

	}

	private val callAnalyser = CallAnalyser
	callAnalyser.classFiles = classFiles
	callAnalyser.classHierarchy = classHierarchy

	behavior of "callAnalyser"
	
	it should "analyse 1 class implementing cloneable but not overriding it" in {
		val result = callAnalyser.cloneableNoClone(classH)
		assert(result.size == 1)
	}

	it should "analyse 1 case not calling super.clone()" in {
		val result = callAnalyser.clonableWithoutSuperClone(classI)
		assert(result.size == 1)
	}

	//TODO REALLY?
	it should "analyse 1 case not implementing cloneable" in {
		val result = callAnalyser.cloneButNotCloneable(classJ)
		assert(result.size == 0)
	}

	it should "analyse 1 case overriding compareTO" in {
		val result = callAnalyser.covariantCompareToMethods(classK)
		assert(result.size == 1)
	}

	it should "analyse 2 garbage collection calls" in {
		val result = callAnalyser.garbageCollectingMethods(classL)
		assert(result.size == 2)
	}

	it should "analyse 2 calls to runFinalizerOnExit(_) " in {
		val result = callAnalyser.methodsThatCallRunFinalizersOnExit(classM)
		println(result.size)
		assert(result.size == 2)
	}

	it should "analyse 1 abstract equals method " in {
		val result = callAnalyser.abstractCovariantEquals(classN)
		assert(result.size == 1)
	}

	it should "analyse 1 class with a public finalize method " in {
		val result = callAnalyser.classesWithPublicFinalizeMethods(classO)
		assert(result.size == 1)
	}
	
	it should "analyse 1 class it superclass not a default constructor and serializable " in {
		val result = callAnalyser.classesWithPublicFinalizeMethods(classP)
		assert(result.size == 1)
	}

}