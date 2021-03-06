/* License (BSD Style License):
*  Copyright (c) 2009, 2012
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
package dependency

import resolved._
import resolved.reader.Java6Framework
import DependencyType._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.FlatSpec
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.ShouldMatchers

/**
  * Tests that the dependency extractor does not report dependencies to primitive types and arrays of primitive types.
  *
  * @author Michael Eichberg
  */
@RunWith(classOf[JUnitRunner])
class DependenciesToPrimitiveTypesTest extends FlatSpec with ShouldMatchers /*with BeforeAndAfterAll*/ {

   //
   //
   // Set up
   //
   //
   val extractedTypes = scala.collection.mutable.Set[Type]()

   object TypesCollector extends SourceElementIDs {
      def sourceElementID(t : Type) : Int = {
         extractedTypes += t
         -1
      }

      def sourceElementID(definingObjectType : ObjectType, fieldName : String) : Int = {
         extractedTypes += definingObjectType
         -1
      }

      def sourceElementID(definingReferenceType : ReferenceType, methodName : String, methodDescriptor : MethodDescriptor) : Int = {
         extractedTypes += definingReferenceType
         -1
      }
   }

   val DependencyCollector = new DependencyExtractor(TypesCollector) with NoSourceElementsVisitor {

      def processDependency(sourceID : Int, targetID : Int, dependencyType : DependencyType) {}
   }

   //
   //
   // EXERCISE
   //
   //
   DependencyCollector.process(
      Java6Framework.ClassFile(ClassLoader.getSystemResource("classfiles/Types.zip").getFile, "types/TypeDeclarations.class")
   );

   //
   //
   // VERIFY
   //
   //

   behavior of "DependencyExtractor"

   it should "extract a dependency to java.lang.Object" in {
      assert(extractedTypes contains ObjectType.Object, "the extractor did no report an existing dependency to java.lang.Object")
   }

   it should "extract a dependency to the type java.lang.Object[]" in {
      assert(extractedTypes contains ArrayType(ObjectType("java/lang/Object")))
   }

   it should "extract a dependency to the type java.lang.Object[][]" in {
      assert(extractedTypes contains ArrayType(ArrayType(ObjectType("java/lang/Object"))))
   }

   it should "not extract dependencies to byte" in {
      assert(!(extractedTypes contains ByteType))
   }

   it should "not extract dependencies to short" in {
      assert(!(extractedTypes contains ShortType))
   }

   it should "not extract dependencies to char" in {
      assert(!(extractedTypes contains CharType))
   }

   it should "not extract dependencies to int" in {
      assert(!(extractedTypes contains IntegerType))
   }

   it should "not extract dependencies to long" in {
      assert(!(extractedTypes contains LongType))
   }

   it should "not extract dependencies to boolean" in {
      assert(!(extractedTypes contains BooleanType))
   }

   it should "not extract dependencies to float" in {
      assert(!(extractedTypes contains FloatType))
   }

   it should "not extract dependencies to double" in {
      assert(!(extractedTypes contains DoubleType))
   }

   it should "not extract dependencies to byte arrays" in {
      assert(!(extractedTypes contains ArrayType(ByteType)))
   }

   it should "not extract dependencies to short arrays" in {
      assert(!(extractedTypes contains ArrayType(ShortType)))
   }

   it should "not extract dependencies to char arrays" in {
      assert(!(extractedTypes contains ArrayType(CharType)))
   }

   it should "not extract dependencies to int arrays" in {
      assert(!(extractedTypes contains ArrayType(IntegerType)))
   }

   it should "not extract dependencies to long arrays" in {
      assert(!(extractedTypes contains ArrayType(LongType)))
   }

   it should "not extract dependencies to boolean arrays" in {
      assert(!(extractedTypes contains ArrayType(BooleanType)))
   }

   it should "not extract dependencies to float arrays" in {
      assert(!(extractedTypes contains ArrayType(FloatType)))
   }

   it should "not extract dependencies to double arrays" in {
      assert(!(extractedTypes contains ArrayType(DoubleType)))
   }

}