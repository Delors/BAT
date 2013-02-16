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
package de.tud.cs.st.bat.resolved.analyses.interprocedural

import de.tud.cs.st.bat.resolved.analyses.Project
import de.tud.cs.st.bat.resolved.{ObjectType, ClassFile, Method, MethodInvocationInstruction}
import collection.mutable

/**
 *
 * @author Ralf Mitschke
 *
 */

object CallGraphClosure
    extends (Project => Iterable[(MethodReference, MethodReference)])
{

    def apply(project: Project) = {
        val cg: mutable.HashMap[MethodReference, mutable.HashSet[MethodReference]] =
            mutable.HashMap.empty

        for {classFile ← project.classFiles
             method ← classFile.methods
        }
        {
            // TODO check what CHA really computes
            addMethodsRecursively (project, classFile, method)(cg)
        }



        for {method ← cg.keys
             target ← cg (method)
        } yield
        {
            (method, target)
        }
    }


    def addMethodsRecursively(project: Project,
                              classFile: ClassFile,
                              method: Method)
                             (implicit cg: mutable.HashMap[MethodReference, mutable.HashSet[MethodReference]])
    {

        val sourceMethodRef = new MethodReference (classFile, method)
        // this method has no body and will never reach any other methods
        if (!method.body.isDefined)
        {
            return //mutable.HashSet.empty
        }
        // the called methods of this method were already computed
        if (cg.contains (sourceMethodRef))
        {
            return //cg (sourceMethodRef)
        }

        val calledMethods = cg.getOrElseUpdate (sourceMethodRef, mutable.HashSet.empty)

        for (instruction ← method.body.get.instructions.filter (_.isInstanceOf[MethodInvocationInstruction]))
        {
            val invoke = instruction.asInstanceOf[MethodInvocationInstruction]
            val targetMethodRef = new MethodReference (invoke)
            // add the call to the set of called methods
            calledMethods.add (targetMethodRef)

            if (cg.contains (targetMethodRef))
            {
                cg (targetMethodRef).foreach (m => calledMethods.add (m))
            }
            else if (invoke.declaringClass.isObjectType && project.classes.contains (invoke.declaringClass.asInstanceOf[ObjectType])) {
                // we want to analyze the further called methods of the target
                val targetClass = project.classes (invoke.declaringClass.asInstanceOf[ObjectType])
                // TODO iterating over all methods is inefficient.
                val targetMethod = targetClass.methods.find (method =>
                    method.name == invoke.name && method.descriptor == invoke.methodDescriptor
                )
                if (targetMethod.isDefined) {
                    addMethodsRecursively (project, targetClass, targetMethod.get)
                    cg.getOrElse (targetMethodRef, mutable.HashSet.empty).foreach (m => calledMethods.add (m))
                }
            }


        }
    }
}