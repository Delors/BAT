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
package de.tud.cs.st.bat.resolved
package dependency
import scala.collection.mutable.HashMap

/**
 * Associates a source element (type, method or field declaration) with a unique id.
 *
 * Types are associated with ids larger than 0 and smaller than one billion.
 *
 * Fields are associated with ids >= 100 000 000 and < 1 000 000 000
 *
 * Methods are associated with ids >= 1 000 000 000
 *
 * Negative IDs are never assigned my this source element to ID mapping.
 * The largest id is equivalent to 1 000 000 000 + number of methods seen.
 *
 * '''Implementation Note'''
 * This class is not thread safe.
 *
 * @author Michael Eichberg
 * @author Thomas Schlosser
 */
trait SourceElementIDsMap extends SourceElementIDs with IDResetter {

    //
    // Associates each type with a unique ID
    //

    val LOWEST_TYPE_ID: Int = 0

    import scala.collection.mutable.WeakHashMap

    private var nextTypeID = LOWEST_TYPE_ID;

    private val typeIDs = WeakHashMap[Type, Int]()

    def sourceElementID(t: Type): Int = typeIDs.getOrElseUpdate(t, { val id = nextTypeID; nextTypeID += 1; id })

    //
    // Associates each field with a unique ID
    //

    val LOWEST_FIELD_ID: Int = 100000000

    private var nextFieldID = LOWEST_FIELD_ID

    private val fieldIDs = WeakHashMap[ObjectType, WeakHashMap[String, Int]]()

    def sourceElementID(definingObjectType: ObjectType, fieldName: String): Int =
        fieldIDs.
            getOrElseUpdate(definingObjectType, { WeakHashMap[String, Int]() }).
            getOrElseUpdate(fieldName, { val id = nextFieldID; nextFieldID += 1; id })

    //
    // Associates each method with a unique ID
    //

    val LOWEST_METHOD_ID: Int = 1000000000

    private var nextMethodID = LOWEST_METHOD_ID

    private val methodIDs = WeakHashMap[ObjectType, WeakHashMap[MethodDescriptor, WeakHashMap[String, Int]]]()

    def sourceElementID(definingObjectType: ObjectType, methodName: String, methodDescriptor: MethodDescriptor): Int = {
        methodIDs.
            getOrElseUpdate(definingObjectType, { WeakHashMap[MethodDescriptor, WeakHashMap[String, Int]]() }).
            getOrElseUpdate(methodDescriptor, { WeakHashMap[String, Int]() }).
            getOrElseUpdate(methodName, { val id = nextMethodID; nextMethodID += 1; id })
    }

    def reset {
        nextTypeID = LOWEST_TYPE_ID
        typeIDs.clear()

        nextFieldID = LOWEST_FIELD_ID
        fieldIDs.clear()

        nextMethodID = LOWEST_METHOD_ID
        methodIDs.clear()
    }

}
