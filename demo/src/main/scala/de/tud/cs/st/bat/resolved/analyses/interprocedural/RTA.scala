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
import de.tud.cs.st.bat.resolved._

import collection.mutable

/**
 *
 * @author Ralf Mitschke
 *
 */

object RTA
    extends (Project => Iterable[(MethodReference, MethodReference)])
{

    def apply(project: Project) = {
        implicit val index: mutable.Map[MethodReference, Set[MethodReference]] =
            mutable.HashMap.empty
        //buildSubtypeMethodIndex (project)

        val createdTypes = buildCreateMethodIndex (project)

        for {classFile ← project.classFiles
             method ← classFile.methods
             if method.body.isDefined
             source = new MethodReference (classFile, method)
             instruction ← method.body.get.instructions.filter (_.isInstanceOf[MethodInvocationInstruction])
             targets = getCallTargetsByIndex (instruction.asInstanceOf[MethodInvocationInstruction])(project, createdTypes, index)
             target ← targets
        } yield (source, target)
    }


    private def getCallTargetsByIndex(instruction: MethodInvocationInstruction)(implicit project: Project, createdTypes: Set[ObjectType], index: mutable.Map[MethodReference, Set[MethodReference]]): Set[MethodReference] =
    {
        instruction match {
            case INVOKESPECIAL (declaringClass, name, methodDescriptor) =>
                Set (MethodReference (declaringClass, name, methodDescriptor))
            case INVOKESTATIC (declaringClass, name, methodDescriptor) =>
                Set (MethodReference (declaringClass, name, methodDescriptor))
            case INVOKEINTERFACE (declaringClass, name, methodDescriptor) =>
                getSubClassMethodsByIndex (declaringClass, name, methodDescriptor)
            case INVOKEVIRTUAL (declaringClass, name, methodDescriptor) =>
                getSubClassMethodsByIndex (declaringClass, name, methodDescriptor)
            case _ => throw new IllegalStateException (instruction + " is not a valid call instruction")
        }
    }

    private def getSubClassMethodsByIndex(declaringType: ReferenceType,
                                          name: String,
                                          methodDescriptor: MethodDescriptor)(implicit project: Project, createdTypes: Set[ObjectType], index: mutable.Map[MethodReference, Set[MethodReference]]): Set[MethodReference] =
    {

        val key = MethodReference (declaringType, name, methodDescriptor)

        if (index.isDefinedAt (key))
            return index (key)

        if (!declaringType.isObjectType)
            return index.getOrElseUpdate (key, Set (key))

        val declaringClass = declaringType.asInstanceOf[ObjectType]
        val subtypes = project.classHierarchy.subtypes (declaringClass)
        if (!subtypes.isDefined)
            return index.getOrElseUpdate (key, Set (key))



        val subtypeMethods =
            for {subType <- subtypes.get
                 if createdTypes.contains (subType)
                 method = findMethod (subType, name, methodDescriptor)
                 if (method.isDefined)
            } yield new MethodReference (subType, method.get)

        index.getOrElseUpdate (key, subtypeMethods + key)
    }


    private def buildCreateMethodIndex(implicit project: Project): Set[ObjectType] =
    {
        var result: Set[ObjectType] = Set.empty

        for {classFile ← project.classFiles
             method ← classFile.methods
             if method.body.isDefined
             NEW (targetType) ← method.body.get.instructions
        }
        {
            result += targetType
        }

        result
    }

    private def findMethod(declaringClass: ObjectType, name: String, methodDescriptor: MethodDescriptor)(implicit project: Project): Option[Method] = {
        methodCache.getOrElseUpdate (
        (declaringClass, name, methodDescriptor),
        {
            val targetClass = project.classes (declaringClass)
            targetClass.methods.find (method =>
                method.name == name && method.descriptor == methodDescriptor
            )
        }
        )
    }

    private val methodCache: mutable.Map[(ObjectType, String, MethodDescriptor), Option[Method]] = mutable.HashMap.empty

}