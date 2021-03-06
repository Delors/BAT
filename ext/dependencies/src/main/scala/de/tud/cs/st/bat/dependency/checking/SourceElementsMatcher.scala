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
package de.tud.cs.st.bat
package dependency
package checking

import resolved._
import scala.collection.immutable.SortedSet

/**
 * A source element matcher determines a set of source elements that matches a given query.
 *
 * @author Michael Eichberg
 */
trait SourceElementsMatcher { left ⇒

    def extension(project: Project): SortedSet[SourceElementID]

    def and(right: SourceElementsMatcher): SourceElementsMatcher = {
        new SourceElementsMatcher {
            def extension(project: Project) = {
                left.extension(project) ++ right.extension(project)
            }

            override def toString() = { //
                "("+left+" and "+right+")"
            }
        }
    }

    def except(right: SourceElementsMatcher): SourceElementsMatcher = {
        new SourceElementsMatcher {
            def extension(project: Project) = {
                left.extension(project) -- right.extension(project)
            }

            override def toString() = { //
                "("+left+" except "+right+")"
            }
        }
    }
}

/**
 * Matches all classes and their members that are declared in the specified package.
 *
 * @param packageName The name of a package in binary notation. (I.e., "/" are used to separate
 * a package name's segments; e.g., "java/lang/Object").
 * @param matchSubpackages If true, all packages that start with the given package name are matched otherwise
 *  only classes declared in the given package are matched.
 *
 * @author Michael Eichberg
 */
case class PackageNameBasedMatcher(val packageName: String, val matchSubpackages: Boolean = false)
        extends SourceElementsMatcher {

    require(packageName.indexOf('*') == -1)
    require(packageName.indexOf('.') == -1)

    def extension(project: Project): SortedSet[SourceElementID] = {
        var sourceElementIDs: SortedSet[SourceElementID] = SortedSet()
        project.classFiles.values.filter((classFile) ⇒ {
            val thisClassPackageName = classFile.thisClass.packageName
            thisClassPackageName.startsWith(packageName) && (
                matchSubpackages ||
                thisClassPackageName.length() == packageName.length()
            )
        }).foreach((classFile) ⇒ {
            sourceElementIDs += project.sourceElementID(classFile)
            sourceElementIDs ++= classFile.methods.map(project.sourceElementID(classFile, _))
            sourceElementIDs ++= classFile.fields.map(project.sourceElementID(classFile, _))
        })
        sourceElementIDs
    }

    override def toString = {
        var s = "\""+packageName.replace('/', '.')+".*"
        if (matchSubpackages)
            s += "*"
        s += "\""
        s
    }
}

case class ClassMatcher(val className: String, val matchPrefix: Boolean = false) extends SourceElementsMatcher {

    require(className.indexOf('*') == -1)
    require(className.indexOf('.') == -1)

    def extension(project: Project): SortedSet[SourceElementID] = {
        var sourceElementIDs: SortedSet[SourceElementID] = SortedSet()
        project.classFiles.values.filter((classFile) ⇒
            {
                val otherClassName = classFile.thisClass.className
                otherClassName.startsWith(className) && (
                    matchPrefix ||
                    otherClassName.length == className.length)
            }).foreach((classFile) ⇒ {
            sourceElementIDs += project.sourceElementID(classFile)
            sourceElementIDs ++= classFile.methods.map(project.sourceElementID(classFile, _))
            sourceElementIDs ++= classFile.fields.map(project.sourceElementID(classFile, _))
        })
        sourceElementIDs
    }

    override def toString = "\""+className+"\""
}

case object NoSourceElementsMatcher extends SourceElementsMatcher {
    def extension(project: Project): SortedSet[SourceElementID] = SortedSet();
}

