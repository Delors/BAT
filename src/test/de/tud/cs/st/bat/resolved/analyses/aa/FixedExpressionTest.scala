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
package de.tud.cs.st.bat.resolved.analyses.aa
import org.scalatest.FunSuite
import de.tud.cs.st.bat.resolved.reader.Java6Framework
import de.tud.cs.st.bat.resolved.analyses.AccessAnalyser

/**
 * @author Dennis Siebert
 */
//@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class FixedExpressionTest extends FunSuite {

	private val classA = Java6Framework.ClassFile("test/classfiles/AA.zip", "aa/fixed_expression/AlwaysIfTrue.class")
	assert(classA ne null)
	
	private val classB = Java6Framework.ClassFile("test/classfiles/AA.zip", "aa/fixed_expression/AlwaysIfFalse.class")
	assert(classB ne null)

	private val classC = Java6Framework.ClassFile("test/classfiles/AA.zip", "aa/fixed_expression/AlwaysWhileTrue.class")
	assert(classC ne null)
	
	private val classD = Java6Framework.ClassFile("test/classfiles/AA.zip", "aa/fixed_expression/AlwaysIfVariable.class")
	assert(classD ne null)
	
	private val classE = Java6Framework.ClassFile("test/classfiles/AA.zip", "aa/fixed_expression/AlwaysWhileVariableTrue.class")
	assert(classE ne null)
	private val accessAnalyser = AccessAnalyser

//	test("URL array") {
//
//		val result = accessAnalyser.FixedExpresson(classA)
//	
//	}
//	
//	test("URL array2") {
//
//		val result = accessAnalyser.FixedExpresson(classB)
//	
//	}
	
	test("URL array3") {

		val result = accessAnalyser.FixedExpresson(classC)
	
	}
	
//	test("URL array4") {
//
//		val result = accessAnalyser.FixedExpresson(classD)
//	
//	}
	
	test("URL array5") {

		val result = accessAnalyser.FixedExpresson(classE)
	
	}
}