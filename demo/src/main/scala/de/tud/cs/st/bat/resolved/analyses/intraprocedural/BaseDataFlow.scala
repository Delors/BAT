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
package de.tud.cs.st.bat.resolved.analyses.intraprocedural

import de.tud.cs.st.bat.resolved.{Method, ObjectType, Instruction}
import structure._

/**
 *
 * @author Ralf Mitschke
 *
 */

object BaseDataFlow
{


    private def startValue(method: Method): State = {
        val code = method.body.get
        var stacks = Stacks (code.maxStack, Nil).addStack ()

        var lvs = if (method.isStatic)
                      LocVariables (Array.fill[Item](code.maxLocals)(Item (ItemType.None, -1, Item.FLAG_IS_NOT_INITIALIZED)))
                  else
                      LocVariables (Array.fill[Item](code.maxLocals)(Item (ItemType.None, -1, Item.FLAG_IS_NOT_INITIALIZED))).setVar (0, Item (ItemType.SomeRef (ObjectType.Class), -1, Item.FLAG_IS_PARAMETER))

        var i: Int = if (method.isStatic) -1 else 0

        for (t <- method.parameterTypes) {
            i = i + 1
            lvs = lvs.setVar (i, Item (ItemType.fromType (t), -1, Item.FLAG_IS_PARAMETER))
        }

        State (stacks, lvs)
    }

    private def emptyValue(method: Method): State = {
        State.createEmptyState (method.body.get.maxStack, method.body.get.maxLocals)
    }


    private def apply(method: Method, cfg: IndexedSeq[List[Int]]): Array[State] = {
        val code = method.body.get

        //The start value of the analysis.
        val sv = startValue (method)
        val ev = emptyValue (method)

        //Initialize the newResult array with the empty value.
        val results: Array[State] = Array.ofDim (cfg.length)
        //Indicator for the fixed point.
        var resultsChanged = true

        //Iterates until fixed point is reached.
        while (resultsChanged) {
            resultsChanged = false

            var pc: Int = 0
            //Iterates over all program counters.
            while (pc < cfg.length && pc >= 0) {

                //The predecessors for the instruction at program counter pc.
                val preds: List[Int] = cfg (pc)

                //Result for this iteration for the instruction at program counter pc.
                var result = sv

                //Initializes the results array.

                //If the instruction has no predecessors, the newResult will be the start value (sv)
                if (preds.length != 0) {

                    //Result = transform the results at the entry labels with their transformer then combine them for a new newResult.
                    result = transform (preds.head, code.instructions, fromArray (results, preds.head, ev))
                    for (i <- 1 until preds.length) {
                        result = (transform (preds (i), code.instructions, fromArray (results, preds (i), ev))).combineWith (result)
                    }
                }

                //Check if the newResult has changed. If no newResult was changed during one iteration, the fixed point has been found.
                if (result != (results (pc))) {
                    resultsChanged = true
                }

                //Set the new newResult in the newResult array.
                results (pc) = result
                //Set the next program counter.
                pc = code.instructions (pc).indexOfNextInstruction (pc, code)

            }
        }

        results

    }

    private def transform(fromPC: Int, a: IndexedSeq[Instruction], currentResult: State): State = {
        BytecodeTransformer (currentResult, fromPC, a (fromPC))
    }

    private def fromArray(currentState: IndexedSeq[State], index: Int, default: State) = {
        if (currentState (index) == null)
            default
        else
            currentState (index)
    }

}
