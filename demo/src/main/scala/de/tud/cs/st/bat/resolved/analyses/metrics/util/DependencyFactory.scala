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
package de.tud.cs.st.bat.resolved.analyses.metrics.util

import de.tud.cs.st.bat.resolved._
import de.tud.cs.st.bat.resolved.analyses.Project
import collection._
import de.tud.cs.st.bat.resolved.INVOKESPECIAL
import de.tud.cs.st.bat.resolved.GETSTATIC
import de.tud.cs.st.bat.resolved.INVOKESTATIC
import de.tud.cs.st.bat.resolved.INVOKEINTERFACE
import de.tud.cs.st.bat.resolved.INVOKEVIRTUAL
import de.tud.cs.st.bat.resolved.PUTSTATIC
import de.tud.cs.st.bat.resolved.GETFIELD
import de.tud.cs.st.bat.resolved.NEW
import de.tud.cs.st.bat.resolved.PUTFIELD

/**
 *
 * @author Ralf Mitschke
 *
 */

object DependencyFactory
{

    def apply(project: Project): mutable.HashSet[Dependency] = {
        val result = new mutable.HashSet[Dependency]
        for (
            cf <- project.classFiles
        )
        {
            val thisType = cf.thisClass

            if (cf.superClass.isDefined) {
                result.add (Dependency (thisType, cf.superClass.get))
            }

            cf.interfaces.foreach (i =>
                result.add (Dependency (thisType, i))
            )

            for (f <- cf.fields if f.fieldType.isObjectType) {
                result.add (Dependency (thisType, f.fieldType.asInstanceOf[ObjectType]))
            }
            for (m <- cf.methods) {
                for (t <- m.parameterTypes if t.isObjectType) {
                    result.add (Dependency (thisType, t.asInstanceOf[ObjectType]))
                }

                if (m.returnType.isObjectType) {
                    result.add (Dependency (thisType, m.returnType.asInstanceOf[ObjectType]))
                }

                if (m.exceptionTable.isDefined) {
                    for (ex <- m.exceptionTable.get.exceptions)
                        result.add (Dependency (thisType, ex))
                }
                if (m.body.isDefined) {
                    for (i ← m.body.get.instructions if i != null) {
                        (i.opcode: @annotation.switch) match {

                            case 178 ⇒ {
                                val GETSTATIC (declaringClass, name, fieldType) = i.asInstanceOf[GETSTATIC]
                                result.add (Dependency (thisType, declaringClass))
                            }

                            case 179 ⇒ {
                                val PUTSTATIC (declaringClass, fieldName, fieldType) = i.asInstanceOf[PUTSTATIC]
                                result.add (Dependency (thisType, declaringClass))
                            }

                            case 180 ⇒ {
                                val GETFIELD (declaringClass, name, fieldType) = i.asInstanceOf[GETFIELD]
                                result.add (Dependency (thisType, declaringClass))
                            }

                            case 181 ⇒ {
                                val PUTFIELD (declaringClass, fieldName, fieldType) = i.asInstanceOf[PUTFIELD]
                                result.add (Dependency (thisType, declaringClass))
                            }

                            case 182 ⇒ {
                                val INVOKEVIRTUAL (declaringClass, name, methodDescriptor) = i.asInstanceOf[INVOKEVIRTUAL]
                                if (declaringClass.isObjectType) {
                                    result.add (Dependency (thisType, declaringClass.asInstanceOf[ObjectType]))
                                }
                            }

                            case 183 ⇒ {
                                val INVOKESPECIAL (declaringClass, name, methodDescriptor) = i.asInstanceOf[INVOKESPECIAL]
                                if (declaringClass.isObjectType) {
                                    result.add (Dependency (thisType, declaringClass.asInstanceOf[ObjectType]))
                                }

                            }

                            case 184 ⇒ {
                                val INVOKESTATIC (declaringClass, name, methodDescriptor) = i.asInstanceOf[INVOKESTATIC]
                                if (declaringClass.isObjectType) {
                                    result.add (Dependency (thisType, declaringClass.asInstanceOf[ObjectType]))
                                }

                            }

                            case 185 ⇒ {
                                val INVOKEINTERFACE (declaringClass, name, methodDescriptor) = i.asInstanceOf[INVOKEINTERFACE]
                                if (declaringClass.isObjectType) {
                                    result.add (Dependency (thisType, declaringClass.asInstanceOf[ObjectType]))
                                }

                            }

                            case 187 ⇒ {
                                result.add (Dependency (thisType, i.asInstanceOf[NEW].objectType))
                            }


                            case 192 ⇒ {
                                val target = i.asInstanceOf[CHECKCAST].referenceType
                                if (target.isObjectType) {
                                    result.add (Dependency (thisType, target.asInstanceOf[ObjectType]))
                                }
                            }
                            case _ ⇒
                        }
                    }
                }
            }
        }
        result
    }

}
