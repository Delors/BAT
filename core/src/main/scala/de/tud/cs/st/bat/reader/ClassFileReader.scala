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
package reader

import java.io.{InputStream, DataInputStream, ByteArrayInputStream}
import java.util.zip.{ZipFile, ZipEntry}

/**
 * Abstract trait that implements a template method to read in a Java class file.
 *
 * This library supports class files from version 45 (Java 1.1) up to
 * (including) version 50 (Java 6). Version 51 (Java 7) is currently only
 * partially supported.
 *
 * '''Format'''
 * {{{
 * ClassFile {
 * 	u4 magic;
 * 	u2 minor_version;
 * 	u2 major_version;
 * 	u2 constant_pool_count;
 * 	cp_info constant_pool[constant_pool_count-1];
 * 	u2 access_flags;
 * 	u2 this_class;
 * 	u2 super_class;
 * 	u2 interfaces_count;
 * 	u2 interfaces[interfaces_count];
 * 	u2 fields_count;
 * 	field_info fields[fields_count];
 * 	u2 methods_count;
 * 	method_info methods[methods_count];
 * 	u2 attributes_count;
 * 	attribute_info attributes[attributes_count];
 * }
 * }}}
 *
 * For details see the JVM Specification: The ClassFile Structure.
 *
 * '''Notes for Implementors'''
 *
 * Reading of the class file's major structures: the constant pool, fields, methods
 * the set of implemented interfaces, and the attributes is
 * delegated to special readers. This enables a very high-level of adaptability.
 *
 * @author Michael Eichberg
 * @author Ralf Mitschke
 */
trait ClassFileReader extends Constant_PoolAbstractions
{

    //
    // ABSTRACT DEFINITIONS
    //

    /**
     * The type of the object that represents a Java class file.
     */
    type ClassFile

    /**
     * The type of the object that represents a class files base information
     */
    type Class_Info

    /**
     * The type of the object that represents a class's fields.
     */
    type Fields

    /**
     * The type of the object that represents all methods of a class.
     */
    type Methods

    /**
     * The type of the object that represents a class declaration's
     * attributes (e.g., the source file attribute.)
     */
    type Attributes

    /**
     * The type of the object that represents the interfaces implemented by
     * a class/interface.
     */
    type Interfaces

    // METHODS DELEGATING TO OTHER READERS
    //

    /**
     * Reads the constant pool using the given stream.
     *
     * The given stream is positioned
     * at the very beginning of the constant pool. This method is called by the
     * template method that reads in a class file to delegate the reading of the
     * constant pool. Only information belonging to the constant pool are allowed
     * to be read. The stream must not be closed after reading the constant pool.
     */
    protected def Constant_Pool(in: DataInputStream): Constant_Pool

    /**
     * Reads the base information about a classfile:
     * access_flags
     * this_class (the identifier of this class' type)
     * super_class (the identifier of the super class' type)
     *
     * The version information is read beforehand and must be passed to this method:
     * minor_version
     * major_version
     */
    protected def Class_Info(minor_version: Int, major_version: Int, in: DataInputStream)(implicit cp: Constant_Pool): Class_Info

    /**
     * Reads the information which interfaces are implemented/extended.
     *
     * The given stream is positioned
     * directly before a class file's "interfaces_count" field. This method is called by the
     * template method that reads in a class file to delegate the reading of the
     * extended interfaces.
     */
    protected def Interfaces(declaringClass: Class_Info, in: DataInputStream, cp: Constant_Pool): Interfaces


    /**
     * Reads all field declarations using the given stream and constant pool.
     *
     * The given stream is positioned
     * directly before a class file's "fields_count" field. This method is called by the
     * template method that reads in a class file to delegate the reading of the
     * declared fields.
     */
    protected def Fields(declaringClass: Class_Info, in: DataInputStream, cp: Constant_Pool): Fields

    /**
     * Reads all method declarations using the given stream and constant pool.
     *
     * The given stream is positioned directly before a class file's "methods_count" field.
     * This method is called by the
     * template method that reads in a class file to delegate the reading of the
     * declared method.
     */
    protected def Methods(declaringClass: Class_Info, in: DataInputStream, cp: Constant_Pool): Methods

    /**
     * Reads all attributes using the given stream and constant pool.
     *
     * '''From the Specification'''
     *
     * The attributes [...] appearing in the attributes table of a ClassFile
     * structure are the InnerClasses, EnclosingMethod, Synthetic, Signature,
     * SourceFile, SourceDebugExtension, Deprecated, RuntimeVisibleAnnotations,
     * RuntimeInvisibleAnnotations, and BootstrapMethods attributes.
     *
     * @note The given stream is positioned directly before a class file's "attributes_count" field.
     *       This method is called by the template method that reads in a class file to delegate the
     *       reading of the attributes.
     */
    protected def Attributes(
                                ap: AttributesParent,
                                cp: Constant_Pool,
                                in: DataInputStream): Attributes

    /**
     * Factory method to create the object that represents the class file
     * as a whole.
     */
    protected def ClassFile(classInfo: Class_Info,
                            interfaces: Interfaces,
                            fields: Fields,
                            methods: Methods,
                            attributes: Attributes)(
        implicit cp: Constant_Pool): ClassFile

    //
    // IMPLEMENTATION
    //

    /**
     * Reads in a class file.
     *
     * @param create a function that is intended to create a new `InputStream` and
     *               which must not return `null`. If you already do have an open input stream
     *               which should not be closed after reading the class file use [[de.tud.cs.st.bat.reader.ClassFileReader.ClassFile( D a t a I n p u t S t r e a m )]] instead.
     *               The (newly created) InputStream returned by calling `create` is closed by this method.
     *               The created input stream will automatically be wrapped by BAT to enable efficient reading of the
     *               class file.
     */
    def ClassFile(create: () ⇒ InputStream): ClassFile = {
        var in = create ();
        if (!in.isInstanceOf[DataInputStream]) {
            // TODO needs to be made more robust
            val data = new Array[Byte](in.available)
            var bytesRead = 0
            while (bytesRead < data.length) {
                bytesRead = bytesRead + in.read (data, bytesRead, data.length - bytesRead)
            }
            if (in.available != 0) throw new RuntimeException ("unexpected additional data available")
            in = new DataInputStream (new ByteArrayInputStream (data))
        }
        try {
            ClassFile (in.asInstanceOf[DataInputStream])
        }
        finally {
            in.close
        }
    }

    protected[this] def ClassFile(zipFile: ZipFile, zipEntry: ZipEntry): ClassFile = {
        ClassFile (() ⇒ zipFile.getInputStream (zipEntry))
    }

    /**
     * Reads in a single class file from a ZIP/Jar file.
     *
     * @param zipFileName the name of an existing ZIP/JAR file that contains class files.
     * @param zipFileEntryName the name of a class file stored in the specified ZIP/JAR file.
     */
    def ClassFile(zipFileName: String, zipFileEntryName: String): ClassFile = {
        val zipFile = new ZipFile (zipFileName)
        try {
            val zipEntry = zipFile.getEntry (zipFileEntryName)
            ClassFile (zipFile, zipEntry)
        }
        finally {
            zipFile.close
        }
    }

    def ClassFiles(zipFile: ZipFile): Seq[ClassFile] = {
        var classFileEntries: List[ZipEntry] = Nil
        val zipEntries = (zipFile).entries
        while (zipEntries.hasMoreElements) {
            val zipEntry = zipEntries.nextElement
            if (!zipEntry.isDirectory && zipEntry.getName.endsWith (".class")) {
                classFileEntries = zipEntry :: classFileEntries
            }
        }
        classFileEntries.view.map (ClassFile (zipFile, _))
    }

    def ClassFiles(zipFileName: String): Seq[ClassFile] = {
        ClassFiles (new ZipFile (zipFileName))
    }

    def ClassFiles(file: java.io.File): Seq[ClassFile] = {
        if (!file.exists ())
            return Nil

        if (file.isFile ()) {
            if (file.getName.endsWith (".zip") || file.getName.endsWith (".jar"))
                return ClassFiles (file.getName)

            if (file.getName.endsWith (".class"))
                return List (ClassFile (() ⇒ new java.io.FileInputStream (file)))

            return Nil
        }

        // file.isDirectory
        var classFiles: List[ClassFile] = Nil
        var directories: List[java.io.File] = List (file) // our work list

        while (directories.nonEmpty) {
            val directory = directories.head
            directories = directories.tail
            var classFileCount = 0
            for (file ← directory.listFiles () /*.par*/ ) {
                if (file.isDirectory ()) {
                    //   directories.synchronized {
                    directories = file :: directories
                    //   }
                }
                else if (file.getName ().endsWith (".class")) {
                    classFileCount += 1
                    val classFile = ClassFile (() ⇒ new java.io.FileInputStream (file))
                    //   classFiles.synchronized {
                    classFiles = classFile :: classFiles
                    //   }
                }
            }
        }

        return classFiles;
    }

    /**
     * Template method to read in a Java class file from the given input stream.
     *
     * @param in the DataInputStream from which the class file will be read. The
     *           stream is not closed by this method.
     */
    def ClassFile(in: DataInputStream): ClassFile = {
        // magic
        require (CLASS_FILE_MAGIC == in.readInt, "No class file.")

        val minor_version = in.readUnsignedShort // minor_version
        val major_version = in.readUnsignedShort // major_version

        // let's make sure that we support this class file's version
        require (major_version >= 45 && // at least JDK 1.1.
            (major_version < 51 ||
                (major_version == 51 && minor_version == 0))) // Java 6 = 50.0; Java 7 == 51.0

        val cp = Constant_Pool (in)

        val ci = Class_Info (minor_version, major_version, in)(cp)
        val interfaces = Interfaces (ci, in, cp)
        val fields = Fields (ci, in, cp)
        val methods = Methods (ci, in, cp)
        val attributes = Attributes (AttributesParents.ClassFile, cp, in)

        ClassFile (
            ci,
            interfaces,
            fields, methods,
            attributes
        )(cp)
    }
}

