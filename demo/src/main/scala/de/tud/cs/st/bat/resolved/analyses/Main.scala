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

import util.perf.{ Counting, PerformanceEvaluation }
import util.graphs.{ Node, toDot }
import reader.Java6Framework

/**
 * Implementation of some simple static analyses to demonstrate the flexibility
 * and power offered by Scala and BAT when analyzing class files.
 *
 * The implemented static analyses are inspired by Findbugs
 * (http://findbugs.sourceforge.net/bugDescriptions.html).
 * <ul>
 * <li>0-FINDBUGS: CI: Class is final but declares protected field (CI_CONFUSED_INHERITANCE) // http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/ConfusedInheritance.java</li>
 * <li>2-FINDBUGS: CN: Class implements Cloneable but does not define or use clone method (CN_IDIOM)</li>
 * <li>2-FINDBUGS: CN: clone method does not call super.clone() (CN_IDIOM_NO_SUPER_CALL)</li>
 * <li>2-FINDBUGS: CN: Class defines clone() but doesn't implement Cloneable (CN_IMPLEMENTS_CLONE_BUT_NOT_CLONEABLE)
 * <li>1-FINDBUGS: Co: Abstract class defines covariant compareTo() method (CO_ABSTRACT_SELF)</li>
 * <li>1-FINDBUGS: Co: Covariant compareTo() method defined (CO_SELF_NO_OBJECT)</li>
 * <li>0-FINDBUGS: Dm: Explicit garbage collection; extremely dubious except in benchmarking code (DM_GC)</li>
 * <li>1-FINDBUGS: Dm: Method invokes dangerous method runFinalizersOnExit (DM_RUN_FINALIZERS_ON_EXIT)</li>
 * <li>1-FINDBUGS: Eq: Abstract class defines covariant equals() method (EQ_ABSTRACT_SELF)</li>
 * <li>0-FINDBUGS: FI: Finalizer should be protected, not public (FI_PUBLIC_SHOULD_BE_PROTECTED)</li>
 * <li>0-FINDBUGS: Se: Class is Serializable but its superclass doesn't define a void constructor (SE_NO_SUITABLE_CONSTRUCTOR)</li>
 * <li>0-FINDBUGS: UuF: Unused field (UUF_UNUSED_FIELD)</li>
 * <li>0-FINDBUGS: (IMSE_DONT_CATCH_IMSE) http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/DontCatchIllegalMonitorStateException.java</li>
 * </ul>
 *
 * @author Michael Eichberg
 */
class Main
object Main extends Main {

    private val CountingPerformanceEvaluator = new PerformanceEvaluation with Counting

    private def printUsage: Unit = {
        println("Usage: java …Main <ZIP or JAR file containing class files>+")
        println("(c) 2011 Michael Eichberg (eichberg@informatik.tu-darmstadt.de)")
    }

    val results = scala.collection.mutable.Map[String,List[Long]]();

    def collect(id: String, time : Long) {
        print(id+", "+time);
        results.update(id,(time/1000l):: results.getOrElse(id, List()));
    }

    def time[T](r: Long ⇒ Unit)(f: ⇒ T): T = {
    	CountingPerformanceEvaluator.time(r)(f)
    }

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

        System.err.println("WARMUP PHASE")
        // for Scalatest - we use 8 warumup runs
        // for Bugs.zip - we use 50 warmup runs
        // for CLASSES.jar - we use 2 warumup runs
        for (i ← 1 to 2) {
            println("\n\n\n\n\n\n\n"+i+"======================================================================="+i);
            //time(t ⇒ println("Performing all analyses took: "+nsToSecs(t))) {
                analyze(args)
            //}
        }
        results.foreach(X => {var(id,times) = X; println(id+","+times.mkString(","))})
        results.clear();

        System.err.println("\n\n\n\nMEASUREMENT PHASE")
        for (i ← 1 to 20) {
            println();//i+"======================================================================="+i);
            time(t ⇒ println("Reading class files and executing all analyses: "+t)) {
               analyze(args)
            }
            System.gc();
            println();
        }
        results.foreach(X => {var(id,times) = X; println(id+","+times.mkString(","))})


        sys.exit(0)
    }

    // The following code is meant to show how easy it is to write analyses;
    // it is not meant to demonstrate how to write such analyses in an efficient
    // manner. (However, the performance is still acceptable.)
    def analyze(zipFiles: Array[String]) {
        var classHierarchy = new ClassHierarchy

        var classFilesCount = 0
        val classFiles = //MemoryUsage(mu ⇒ println("Memory required for the bytecode representation: "+(mu / 1024.0 / 1024.0)+" MByte")) {
            //   time(t ⇒ println("Reading all class files took: "+t/*nsToSecs(t)*/)) {
            for (
                zipFile ← zipFiles; // if { println("Reading: "+zipFile); true };
                classFile ← Java6Framework.ClassFiles(zipFile)
            ) yield {
                classFilesCount += 1
                classHierarchy = classHierarchy + classFile
                classFile
            }
        //   }
        //}
        val getClassFile: Map[ObjectType, ClassFile] = classFiles.map(cf ⇒ (cf.thisClass, cf)).toMap
        //println("Number of class files: "+classFilesCount)

        //MemoryUsage(mu ⇒ println("Memory still used after executing the queries: "+(mu / 1024.0 / 1024.0)+" MByte")) {
            // FINDBUGS: CI: Class is final but declares protected field (CI_CONFUSED_INHERITANCE) // http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/ConfusedInheritance.java
            val protectedFields = time(t ⇒ collect("CI_CONFUSED_INHERITANCE",t /*nsToSecs(t)*/ )) {
                for (
                    classFile ← classFiles if classFile.isFinal;
                    field ← classFile.fields if field.isProtected
                ) yield (classFile, field)
            }
            println(", " /*"\tViolations: "*/ +protectedFields.size)

            // FINDBUGS: CN: Class implements Cloneable but does not define or use clone method (CN_IDIOM)
            var cloneableNoClone = time(t ⇒ collect("CN_IDIOM",t /*nsToSecs(t)*/ )) {
                // Weakness: We will not identify cloneable classes in projects, where we extend a predefined
                // class (of the JDK) that indirectly inherits from Cloneable.
                for {
                    allCloneables ← classHierarchy.subtypes(ObjectType("java/lang/Cloneable")).toList
                    cloneable ← allCloneables
                    classFile ← getClassFile.get(cloneable).toList
                    if !classFile.methods.exists({
                        case Method(_, "clone", MethodDescriptor(Seq(), ObjectType.Object), _) ⇒ true;
                        case _ ⇒ false;
                    })
                } yield classFile.thisClass.className
            }
            println(", "+cloneableNoClone.size)

            // FINDBUGS: CN: clone method does not call super.clone() (CN_IDIOM_NO_SUPER_CALL)
            var cloneDoesNotCallSuperClone = time(t ⇒ collect("CN_IDIOM_NO_SUPER_CALL",t /*nsToSecs(t)*/ )) {
                for {
                    classFile ← classFiles
                    if !classFile.isInterfaceDeclaration && !classFile.isAnnotationDeclaration
                    superClass ← classFile.superClass.toList
                    method @ Method(_, "clone", MethodDescriptor(Seq(), ObjectType.Object), _) ← classFile.methods
                    if !method.isAbstract
                    if !method.body.get.instructions.exists({
                        case INVOKESPECIAL(`superClass`, "clone", MethodDescriptor(Seq(), ObjectType.Object)) ⇒ true;
                        case _ ⇒ false;
                    })
                } yield (classFile /*.thisClass.className*/ , method /*.name*/ )
            }
            println(", " /*"\tViolations: "*/ +cloneDoesNotCallSuperClone.size /*+": "+cloneDoesNotCallSuperClone.mkString("; ")*/ )

            // FINDBUGS: CN: Class defines clone() but doesn't implement Cloneable (CN_IMPLEMENTS_CLONE_BUT_NOT_CLONEABLE)
            var cloneButNotCloneable = time(t ⇒ collect("CN_IMPLEMENTS_CLONE_BUT_NOT_CLONEABLE",t /*nsToSecs(t)*/ )) {
                for {
                    classFile ← classFiles if !classFile.isAnnotationDeclaration && classFile.superClass.isDefined
                    method @ Method(_, "clone", MethodDescriptor(Seq(), ObjectType.Object), _) ← classFile.methods
                    if !classHierarchy.isSubtypeOf(classFile.thisClass, ObjectType("java/lang/Cloneable")).getOrElse(false)
                } yield (classFile.thisClass.className, method.name)
            }
            println(", " /*"\tViolations: "*/ /*+cloneButNotCloneable.mkString(", ")*/ +cloneButNotCloneable.size)

            // FINDBUGS: Co: Abstract class defines covariant compareTo() method (CO_ABSTRACT_SELF)
            // FINDBUGS: Co: Covariant compareTo() method defined (CO_SELF_NO_OBJECT)
            // This class defines a covariant version of compareTo().  To correctly override the compareTo() method in the Comparable interface, the parameter of compareTo() must have type java.lang.Object.
            var covariantCompareToMethods = time(t ⇒ collect("CO_SELF_NO_OBJECT/CO_ABSTRACT_SELF",t /*nsToSecs(t)*/ )) {
                // Weakness: In a project, where we extend a predefined class (of the JDK) that
                // inherits from Comparable and in which we define covariant comparesTo method,
                // we will not be able to identify this issue unless we have identified the whole
                // class hierarchy.
                for {
                    allComparables ← classHierarchy.subtypes(ObjectType("java/lang/Comparable")).toList
                    comparable ← allComparables
                    classFile ← getClassFile.get(comparable).toList
                    method @ Method(_, "compareTo", MethodDescriptor(Seq(parameterType), IntegerType), _) ← classFile.methods
                    if parameterType != ObjectType("java/lang/Object")
                } yield (classFile, method)
            }
            println(", " /*"\tViolations: "*/ +covariantCompareToMethods.size)

            // FINDBUGS: Dm: Explicit garbage collection; extremely dubious except in benchmarking code (DM_GC)
            var garbageCollectingMethods: List[(ClassFile, Method, Instruction)] = Nil
            time(t ⇒ collect("DM_GC",t /*nsToSecs(t)*/ )) {
                for ( // we don't care about gc calls in java.lang and also about gc calls that happen inside of methods related to garbage collection (heuristic)
                    classFile ← classFiles if !classFile.thisClass.className.startsWith("java/lang");
                    method ← classFile.methods if method.body.isDefined && !"(^gc)|(gc$)".r.findFirstIn(method.name).isDefined;
                    instruction ← method.body.get.instructions
                ) {
                    instruction match {
                        case INVOKESTATIC(ObjectType("java/lang/System"), "gc", MethodDescriptor(Seq(), VoidType)) |
                            INVOKEVIRTUAL(ObjectType("java/lang/Runtime"), "gc", MethodDescriptor(Seq(), VoidType)) ⇒
                            garbageCollectingMethods = (classFile, method, instruction) :: garbageCollectingMethods
                        case _ ⇒
                    }
                }
            }
            println(", " /*"\tViolations: "*/ +garbageCollectingMethods.size)

            // FINDBUGS: Dm: Method invokes dangerous method runFinalizersOnExit (DM_RUN_FINALIZERS_ON_EXIT)
            var methodsThatCallRunFinalizersOnExit: List[(ClassFile, Method, Instruction)] = Nil
            time(t ⇒ collect("DM_RUN_FINALIZERS_ON_EXIT",t /*nsToSecs(t)*/ )) {
                for (
                    classFile ← classFiles;
                    method ← classFile.methods if method.body.isDefined;
                    instruction ← method.body.get.instructions
                ) {
                    instruction match {
                        case INVOKESTATIC(ObjectType("java/lang/System"), "runFinalizersOnExit", MethodDescriptor(Seq(BooleanType), VoidType)) |
                            INVOKESTATIC(ObjectType("java/lang/Runtime"), "runFinalizersOnExit", MethodDescriptor(Seq(BooleanType), VoidType)) ⇒
                            methodsThatCallRunFinalizersOnExit = (classFile, method, instruction) :: methodsThatCallRunFinalizersOnExit
                        case _ ⇒
                    }
                }
            }
            println(", " /*"\tViolations: "*/ +methodsThatCallRunFinalizersOnExit.size)
            //methodsThatCallRunFinalizersOnExit.foreach((t) => {println(t._1.thisClass.className+ " "+ t._2.name)});

            //        // FINDBUGS: Eq: Abstract class defines covariant equals() method (EQ_ABSTRACT_SELF)
            //        var abstractClassThatDefinesCovariantEquals = time(t ⇒ println("EQ_ABSTRACT_SELF: "+nsToSecs(t))) {
            //            for (
            //                classFile ← classFiles if classFile.isAbstract;
            //                method @ Method(_, "equals", MethodDescriptor(Seq(parameterType), BooleanType), _) ← classFile.methods if parameterType != ObjectType("java/lang/Object")
            //            ) yield (classFile, method);
            //        }
            //        println("\tViolations: "+abstractClassThatDefinesCovariantEquals.size)
            //        //abstractClassThatDefinesCovariantEquals.foreach((t) => {println(t._1.thisClass.className+ " "+ t._2.name)});
            // FINDBUGS: EQ_ABSTRACT_SELF - a covariant equals method that is abstract (the following reflects the implemented checker)
            var abstractCovariantEquals = time(t ⇒ collect("EQ_ABSTRACT_SELF",t /*nsToSecs(t)*/ )) {
                for (
                    classFile ← classFiles;
                    method @ Method(_, "equals", MethodDescriptor(Seq(classFile.thisClass), BooleanType), _) ← classFile.methods if method.isAbstract
                ) yield (classFile, method);
            }
            println(", " /*"\tViolations: "*/ +abstractCovariantEquals.size)

            // FINDBUGS: FI: Finalizer should be protected, not public (FI_PUBLIC_SHOULD_BE_PROTECTED)
            var classesWithPublicFinalizeMethods = time(t ⇒ collect("FI_PUBLIC_SHOULD_BE_PROTECTED",t /*nsToSecs(t)*/ )) {
                for {
                    classFile ← classFiles
                    if classFile.methods.exists(method ⇒
                        method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0
                    )
                } yield classFile
            }
            println(", " /*"\tViolations: "*/ +classesWithPublicFinalizeMethods.length)

            // FINDBUGS: Se: Class is Serializable but its superclass doesn't define a void constructor (SE_NO_SUITABLE_CONSTRUCTOR)
            val serializableClasses = classHierarchy.subclasses(ObjectType("java/io/Serializable")).getOrElse(Set.empty)
            // The following solution reports all pairs of seriablizable classes and their non-seriablizable
            // superclasses that do not define a default constructor.
            //        val classesWithoutDefaultConstructor = time(t ⇒ println("SE_NO_SUITABLE_CONSTRUCTOR: "+nsToSecs(t))) {
            //            for (
            //                serializableClass ← serializableClasses;
            //                superclasses ← classHierarchy.superclasses(serializableClass)
            //            ) yield for (
            //                superclass ← superclasses if getClassFile.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
            //                    {
            //                        val superClassFile = getClassFile(superclass)
            //                        !superClassFile.isInterfaceDeclaration &&
            //                            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length == 0)
            //                    }
            //            ) yield (serializableClass, superclass)
            //        }
            val classesWithoutDefaultConstructor = time(t ⇒ collect("SE_NO_SUITABLE_CONSTRUCTOR",t /*nsToSecs(t)*/ )) {
                for (
                    superclass ← classHierarchy.superclasses(serializableClasses) if getClassFile.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
                        {
                            val superClassFile = getClassFile(superclass)
                            !superClassFile.isInterfaceDeclaration &&
                                !superClassFile.constructors.exists(_.descriptor.parameterTypes.length == 0)
                        }
                ) yield superclass // there can be at most one method
            }
            println(", " /*"\tViolations: "*/ +classesWithoutDefaultConstructor.size);

            // FINDBUGS: UuF: Unused field (UUF_UNUSED_FIELD)
            var unusedFields: List[(ClassFile, Traversable[String])] = Nil
            time(t ⇒ collect("UUF_UNUSED_FIELD",t /*nsToSecs(t)*/ )) {
                for (classFile ← classFiles if !classFile.isInterfaceDeclaration) {
                    val declaringClass = classFile.thisClass
                    var privateFields = (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet
                    for (
                        method ← classFile.methods if method.body.isDefined;
                        instruction ← method.body.get.instructions
                    ) {
                        instruction match {
                            case GETFIELD(`declaringClass`, name, _)  ⇒ privateFields -= name
                            case GETSTATIC(`declaringClass`, name, _) ⇒ privateFields -= name
                            case _                                    ⇒
                        }
                    }
                    if (privateFields.size > 0)
                        unusedFields = (classFile, privateFields) :: unusedFields
                }
            }
            println(", "+ /*"\tViolations: "+*/ unusedFields.size)
            //            var allFields = List[(ObjectType, String)]()
            //            var readFields = Set[(ObjectType, String)]()
            //            var writtenFields = Set[(ObjectType, String)]()
            //            time(t ",t"UUF_UNUSED_FIELD (public, protected, default and private), "+t/*nsToSecs(t)*/)) {
            //                allFields = for (
            //                    classFile ← classFiles.toList if !classFile.isInterfaceDeclaration;
            //                    field ← classFile.fields if !(field.isStatic && field.isFinal)
            //                ) yield (classFile.thisClass, field.name);
            //                for (
            //                    classFile ← classFiles if !classFile.isInterfaceDeclaration;
            //                    method ← classFile.methods; // if method.body.isDefined;
            //                    body ← method.body;
            //                    instruction ← body.instructions
            //                ) {
            //                    instruction match {
            //                        case GETFIELD(declaringClass, name, _)  ⇒ readFields += ((declaringClass, name))
            //                        case GETSTATIC(declaringClass, name, _) ⇒ readFields += ((declaringClass, name))
            //                        case PUTSTATIC(declaringClass, name, _) ⇒ writtenFields += ((declaringClass, name))
            //                        case PUTFIELD(declaringClass, name, _)  ⇒ writtenFields += ((declaringClass, name))
            //                        case _                                  ⇒
            //                    }
            //                }
            //            }
            //            println("\tNumber of fields: "+allFields.size+"; Number of read fields: "+readFields.size+"; Number of written fields: "+writtenFields.size);
            //            println("\tViolations - unused fields: "+((allFields diff readFields.toSeq) diff writtenFields.toSeq).size)
            //            println("\tViolations - fields that are not read: "+(allFields diff readFields.toSeq).size)
            //            println("\tViolations - fields that are not written: "+(allFields diff writtenFields.toSeq).size)

            // FINDBUGS: (IMSE_DONT_CATCH_IMSE) http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/DontCatchIllegalMonitorStateException.java
            // Dubious catching of IllegalMonitorStateException.
            val IllegalMonitorStateExceptionType = ObjectType("java/lang/IllegalMonitorStateException")
            val catchesIllegalMonitorStateException = time(t ⇒ collect("IMSE_DONT_CATCH_IMSE",t /*nsToSecs(t)*/ )) {
                for (
                    classFile ← classFiles if classFile.isClassDeclaration;
                    method ← classFile.methods if method.body.isDefined;
                    exceptionHandler ← method.body.get.exceptionHandlers if exceptionHandler.catchType == IllegalMonitorStateExceptionType
                ) yield (classFile, method)
            }
            println(", " /*"\tViolations: "*/ +catchesIllegalMonitorStateException.size)
        //}
    }
}