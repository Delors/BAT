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
package de.tud.cs.st.util.perf

import scala.collection.mutable.Map

/**
 * Measures the execution time of some code.
 *
 * @author Michael Eichberg
 */
trait PerformanceEvaluation {

    /**
     * Times the execution of a given method f (function literal / code block).
     *
     * @param r A function that is passed the time (in nano seconds) that it
     * 	took to evaluate the function f.
     */
    def time[T](r: Long ⇒ Unit)(f: ⇒ T): T = {

        val startTime: Long = System.nanoTime
        val result = f
        val endTime: Long = System.nanoTime

        r(endTime - startTime)

        result
    }

    private[this] val times: Map[Symbol, Long] = Map()

    /**
     * Times the execution of the given method / function literal / code block and
     * adds it to the execution time of previous methods / function literals/ code blocks
     * that were measured and for which the same symbol was used. <br/>
     * E.g. <code>time('base_analysis){ ... do something ... }</code>
     *
     * @param s Symbol used to put multiple measurements into relation.
     * @param f The function that will be evaluated and for which the execution
     * time will be measured.
     */
    def time[T](s: Symbol)(f: ⇒ T): T = {
        val startTime = System.nanoTime
        val result = f
        val endTime = System.nanoTime

        val old = times.getOrElseUpdate(s, 0l)
        times.update(s, old + (endTime - startTime))

        result
    }

    def getTime(sym: Symbol): Long = {
        times.getOrElse(sym, 0l)
    }

    def reset(s: Symbol) {
        times.remove(s)
    }

    def resetAll() {
        times.clear()
    }
}

