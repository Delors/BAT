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

import de.tud.cs.st.bat.resolved._
import de.tud.cs.st.bat.resolved.JSR_W
import de.tud.cs.st.bat.resolved.LOOKUPSWITCH
import de.tud.cs.st.bat.resolved.JSR
import de.tud.cs.st.bat.resolved.TABLESWITCH

/**
 *
 * @author Ralf Mitschke
 *
 */

object BaseControlFlow
{

    /**
     * This method computes the preceding program counters based on an Array[Instruction]
     * @param code The underlying code info of the control flow graph.
     * @return The array of transformers of a method. The indexes of the array are the valid program
     *         counters of the program. The transformers on non-PC indexes is null.
     */
    def computePredecessors(code: Code): IndexedSeq[List[Int]] = {

        val instructions = code.instructions
        val result = Array.ofDim[List[Int]](instructions.length)
        var currentPC = 0
        var nextPC = 0

        while (nextPC < instructions.length && nextPC >= 0) {

            nextPC = instructions (currentPC).indexOfNextInstruction (currentPC, code)

            if (nextPC < instructions.length && nextPC >= 0) {
                if (instructions (currentPC).isInstanceOf[ConditionalBranchInstruction]) {
                    addToArray (result, nextPC, currentPC)
                    addToArray (result, currentPC + instructions (currentPC).asInstanceOf[ConditionalBranchInstruction].branchoffset, currentPC)
                }
                else if (instructions (currentPC).isInstanceOf[UnconditionalBranchInstruction]) {
                    addToArray (result, currentPC + instructions (currentPC).asInstanceOf[UnconditionalBranchInstruction].branchoffset, currentPC)
                }
                else if (instructions (currentPC).isInstanceOf[LOOKUPSWITCH]) {
                    val instr = instructions (currentPC).asInstanceOf[LOOKUPSWITCH]
                    for (p <- instr.npairs) {
                        addToArray (result, p._2, currentPC)
                    }
                }
                else if (instructions (currentPC).isInstanceOf[TABLESWITCH]) {
                    val instr = instructions (currentPC).asInstanceOf[TABLESWITCH]
                    for (p <- instr.jumpOffsets) {
                        addToArray (result, p, currentPC)
                    }
                }
                else if (instructions (currentPC).isInstanceOf[ReturnInstruction]) {
                    //There is no control flow from a return instruction.
                }
                else if (instructions (currentPC).isInstanceOf[ATHROW.type]) {
                    //TODO: fill in what exceptions do
                }
                else if (instructions (currentPC).isInstanceOf[JSR]) {
                    addToArray (result, currentPC + instructions (currentPC).asInstanceOf[JSR].branchoffset, currentPC)

                }
                else if (instructions (currentPC).isInstanceOf[JSR_W]) {
                    addToArray (result, currentPC + instructions (currentPC).asInstanceOf[JSR_W].branchoffset, currentPC)
                }
                else
                {
                    addToArray (result, nextPC, currentPC)
                }
            }
            if (result (currentPC) == null) result (currentPC) = Nil
            currentPC = nextPC
        }

        result

    }

    /**
     * This method add an element to a list in an array at a specified index.
     * @param a The array where the lists are stored.
     * @param index The index in the array.
     * @param add The element that should be added to a list.
     */
    private def addToArray(a: Array[List[Int]], index: Int, add: Int) {
        if (a (index) == null)
            a (index) = Nil
        a (index) = add :: a (index)
    }


}
