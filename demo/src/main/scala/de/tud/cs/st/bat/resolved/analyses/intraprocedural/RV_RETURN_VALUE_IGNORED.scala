package de.tud.cs.st.bat.resolved.analyses.intraprocedural

import de.tud.cs.st.bat.resolved._
import analyses.Project



/**
 * @author Ralf Mitschke
 */
object RV_RETURN_VALUE_IGNORED
    extends (Project => Iterable[(ClassFile, Method, Int)])
{

    private def isPopInstruction(instruction: Instruction): Boolean =
        instruction.isInstanceOf[POP.type] || instruction.isInstanceOf[POP2.type]


    def apply(project: Project) = {
        for {classFile ← project.classFiles
             method ← classFile.methods
             if method.body.isDefined
             code = method.body.get
             cfg = BaseControlFlow (code)
             df = BaseDataFlow (method, cfg)
             (invoke, idx) ← code.instructions.zipWithIndex.filter (e => isPopInstruction (e._1))
             stacks = df (idx).s.collection
             stack ← stacks
             if stack.size > 0 && (stack.get (0).isReturnValue || stack.get (0).isCreatedByNew)
        } yield
        {
            (classFile, method, idx)
        }
    }

}
