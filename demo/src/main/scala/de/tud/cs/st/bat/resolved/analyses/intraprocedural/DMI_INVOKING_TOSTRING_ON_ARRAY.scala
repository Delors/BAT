package de.tud.cs.st.bat.resolved.analyses.intraprocedural

import de.tud.cs.st.bat.resolved._
import analyses.Project

/**
 * @author Ralf Mitschke
 */
object DMI_INVOKING_TOSTRING_ON_ARRAY
    extends (Project => Iterable[(ClassFile, Method, Int)])
{

    private def isToString(instr: INVOKEVIRTUAL): Boolean =
        (instr.name == "toString") && (instr.methodDescriptor == MethodDescriptor (Nil, ObjectType.String))

    private def isAppendStringBuilder(instr: INVOKEVIRTUAL): Boolean =
        (instr.name == "append") &&
            (instr.declaringClass == ObjectType ("java/lang/StringBuilder")) &&
            (instr.methodDescriptor == MethodDescriptor (ObjectType.Object :: Nil, ObjectType ("java/lang/StringBuilder")))

    private def isAppendStringBuffer(instr: INVOKEVIRTUAL): Boolean =
        (instr.name == "append") &&
            (instr.declaringClass == ObjectType ("java/lang/StringBuffer")) &&
            (instr.methodDescriptor == MethodDescriptor (ObjectType.Object :: Nil, ObjectType ("java/lang/StringBuffer")))

    private def isPrint(instr: INVOKEVIRTUAL): Boolean =
        ((instr.name == "print") || (instr.name == "println")) &&
            (instr.methodDescriptor == MethodDescriptor (ObjectType.Object :: Nil, VoidType))

    private def invokesToString(instruction: Instruction): Boolean =
    {
        if (!instruction.isInstanceOf[INVOKEVIRTUAL])
            return false

        val invokeVirtual = instruction.asInstanceOf[INVOKEVIRTUAL]
        isToString (invokeVirtual) || isAppendStringBuilder (invokeVirtual) || isAppendStringBuffer (invokeVirtual) || isPrint (invokeVirtual)
    }

    def apply(project: Project) = {
        for {classFile ← project.classFiles
             method ← classFile.methods
             if method.body.isDefined
             code = method.body.get
             //if code.instructions.exists (invokesToString)
             cfg = BaseControlFlow (code)
             df = BaseDataFlow (method, cfg)
             (invoke, idx) ← code.instructions.zipWithIndex.filter (e => invokesToString (e._1))
             stacks = df (idx).s.collection
             stack ← stacks
             if (stack.size >= 1)
             head = stack.get (0)
             if (head.getDeclaredType.isArrayType)
        } yield
        {
            (classFile, method, idx)
        }
    }

}
