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
package de.tud.cs.st.util

import java.io.InputStream

import scala.collection.mutable.WrappedArray

/**
  * Implementation of some control abstractions.
  *
  * @author Michael Eichberg
  */
object ControlAbstractions {

    /**
      * This function takes care of the correct handling of input streams.
      * The function takes a function <code>f</code> that creates a new <code>InputStream</code>
      * and a function <code>r</code> that processes an input stream. If f returns
      * <code>null</code>, <code>null</code> is passed to r.
      */
    def read[I <: InputStream, T](f: ⇒ I)(r: I ⇒ T): T = {
        val in = f // this calls the function f
        try {
            r(in)
        }
        finally {
            if (in != null)
                in.close
        }
    }

    def repeat[T: ClassManifest](times: Int)(f: ⇒ T): WrappedArray[T] = {
        val array = new Array[T](times)
        var i = 0
        while (i < times) {
            array(i) = f
            i += 1
        }
        WrappedArray.make[T](array)
    }

    def repeat[T: ClassManifest](f: () ⇒ T)(times: Int): WrappedArray[T] = {
        repeat(times)(f())
    }

    /**
      * @param f a function f that is evaluated
      * @param times the number of times the result of evaluating f <b>once</b> is stored
      * in an array of size times.
      */
    def repeatResultOfEvaluation[T: ClassManifest](f: ⇒ T)(times: Int): WrappedArray[T] = {
        repeat(times)(f)
    }
}