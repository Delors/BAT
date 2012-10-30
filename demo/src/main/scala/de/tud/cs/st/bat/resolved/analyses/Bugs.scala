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
package de.tud.cs.st
package bat.resolved
package analyses

import reader.Java6Framework

/**
  * @author Michael Eichberg
  */
object Bugs {

    private def printUsage: Unit = {
        println("Usage: java …Bugs <ZIP or JAR file containing class files>+")
        println("(c) 2012 Michael Eichberg, Ralf Mitschke")
    }

    val analyses = List(
        DP_DO_INSIDE_DO_PRIVILEGED,
        NonSerializableClassHasASerializableInnerClass,
        FI_USELESS,
        UG_SYNC_SET_UNSYNC_GET
    )

    def main(args: Array[String]) {

        if (args.length == 0 || !args.forall(arg ⇒ arg.endsWith(".zip") || arg.endsWith(".jar"))) {
            printUsage
            sys.exit(1)
        }

        for (arg ← args) {
            val file = new java.io.File(arg)
            if (!file.canRead() || file.isDirectory()) {
                println("The file: "+file+" cannot be read.");
                printUsage
                sys.exit(1)
            }
        }

        println("Reading class files:")
        var project = new Project()
        for {
            zipFile ← args if {
                println("\t"+zipFile); true
            };
            classFile ← Java6Framework.ClassFiles(zipFile)
        } yield {
            project += classFile
        }
        println("Starting analyses: ")

        for (analysis ← analyses) {
            print(analysis.getClass().getSimpleName()+" : \n")
            println(analysis.analyze(project).mkString("\n"))
        }
    }

}