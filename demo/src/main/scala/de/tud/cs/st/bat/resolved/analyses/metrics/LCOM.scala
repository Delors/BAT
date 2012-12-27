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
package de.tud.cs.st.bat.resolved.analyses.metrics

import de.tud.cs.st.bat.resolved._
import de.tud.cs.st.bat.resolved.analyses.Project


/**
 *
 * @author Ralf Mitschke
 *
 */

object LCOM
    extends (Project => Iterable[(ObjectType, Option[Double])])
{

    def isNeitherConstructOrStaticInitializer(m: Method) = m.name != "<init>" && m.name != "<cinit>"

    def apply(project: Project) =
        for (classFile <- project.classFiles) yield {
            val numberOfFields = classFile.fields.size
            val relevantMethods = for (m <- classFile.methods if isNeitherConstructOrStaticInitializer (m)) yield m
            val numberOfMethods = relevantMethods.size

            val classType = classFile.thisClass
            val numberOfFieldAccesses =
                (for {method <- relevantMethods
                      if method.body.isDefined
                      instruction <- method.body.get.instructions
                      if instruction.isInstanceOf[FieldAccessInstruction]
                      if (
                          instruction match {
                              case GETFIELD (`classType`, _, _) => true
                              case PUTFIELD (`classType`, _, _) => true
                              case GETSTATIC (`classType`, _, _) => true
                              case PUTSTATIC (`classType`, _, _) => true
                              case _ => false
                          }
                          )
                } yield (instruction)).size

            val result: (ObjectType, Option[Double]) =
                if (numberOfFields == 0 || numberOfFields == 1 || numberOfMethods == 0)
                    (classType, None)
                else
                {
                    val a: Double = 1.asInstanceOf[Double] / numberOfFields.asInstanceOf[Double]
                    val b: Double = a * numberOfFieldAccesses.asInstanceOf[Double] - numberOfMethods.asInstanceOf[Double]
                    val c: Double = 1 - numberOfMethods
                    if (c == 0)
                        (classType, None)
                    else
                    {
                        val d: Double = b / c
                        if (d == 0 || d == -0)
                            (classType, Some (0))
                        else if (d < 0)
                                 (classType, None)
                        else (classType, Some (d))
                    }
                }
            result
        }
}
