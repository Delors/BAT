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
package de.tud.cs.st.bat
package resolved

import reader.Java6Framework

import org.scalatest.FunSuite

/**
 * @author Michael Eichberg
 */
//@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class AttributesTest extends FunSuite {

    test("test that the deprecated attribute is present") {
        val classFile1 = Java6Framework.ClassFile("test/classfiles/Attributes.zip", "attributes/DeprecatedByAnnotation.class")
        assert(classFile1.isDeprectated)
        assert(classFile1.runtimeVisibleAnnotations.get.find({ case Annotation(ObjectType("java/lang/Deprecated"), _) ⇒ true; case _ ⇒ false }) != None)

        val classFile2 = Java6Framework.ClassFile("test/classfiles/Attributes.zip", "attributes/DeprecatedByJavaDoc.class")
        assert(classFile1.isDeprectated)

    }

    test("test that the source file attribute is present") {
        val classFile1 = Java6Framework.ClassFile("test/classfiles/Attributes.zip", "attributes/DeprecatedByAnnotation.class")
        assert(classFile1.sourceFile != None)
    }

}
