package de.tud.cs.st.bat.resolved.analyses.intraprocedural

import de.tud.cs.st.bat.resolved._
import de.tud.cs.st.bat.resolved.analyses.Project
import structure._

/**
 * @author Ralf Mitschke
 */
object SQL_BAD_PREPARED_STATEMENT_ACCESS
    extends (Project => Iterable[(ClassFile, Method, Int)])
{

    private val SUFFIX_LIST: List[String] = "Array" :: "AsciiStream" :: "BigDecimal" :: "BinaryStream" ::
        "Blob" :: "Boolean" :: "Byte" :: "Bytes" :: "CharacterStream" :: "Clob" :: "Date" :: "Double" ::
        "Float" :: "Int" :: "Long" :: "Object" :: "Ref" :: "RowId" :: "Short" :: "String" :: "Time" :: "Timestamp" ::
        "UnicodeStream" :: "URL" :: Nil


    def callsPreparedStatementSet(instruction: Instruction): Boolean = {
        if (!instruction.isInstanceOf[INVOKEINTERFACE])
            return false

        val invoke = instruction.asInstanceOf[INVOKEINTERFACE]
        invoke.declaringClass == ObjectType ("java/sql/PreparedStatement") &&
            invoke.name.size > 3 &&
            invoke.name.startsWith ("set") &&
            SUFFIX_LIST.contains (invoke.name.substring (3))
    }

    def apply(project: Project) = {
        for {classFile ← project.classFiles
             method ← classFile.methods
             code ← method.body
             if code.instructions.exists (callsPreparedStatementSet)
             cfg = BaseControlFlow (code)
             df = BaseDataFlow (method, cfg)
             (call, idx) ← code.instructions.zipWithIndex.filter (e => callsPreparedStatementSet (e._1))
             stacks = df (idx).s.collection
             stack ← stacks
             if isBadAccess (call, stack)
        } yield
        {
            (classFile, method, idx)
        }
    }

    private def isBadAccess(instruction: Instruction, stack: Stack): Boolean = {
        val invInstr = instruction.asInstanceOf[INVOKEINTERFACE]
        val numParams: Int = invInstr.methodDescriptor.parameterTypes.size
        val indexParam: Item = stack.get (numParams - 1)

        indexParam.isCouldBeZero
    }

}
