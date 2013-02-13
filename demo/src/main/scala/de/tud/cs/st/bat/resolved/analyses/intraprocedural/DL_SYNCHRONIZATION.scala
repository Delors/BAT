package de.tud.cs.st.bat.resolved.analyses.intraprocedural

import de.tud.cs.st.bat.resolved._
import analyses.Project

/**
 * @author Ralf Mitschke
 */
object DL_SYNCHRONIZATION
    extends (Project => Iterable[(ClassFile, Method, Int)])
{

    private val BAD_SIGNATURES: List[Type] =
        ObjectType ("java/lang/Boolean") ::
            ObjectType ("java/lang/Byte") ::
            ObjectType ("java/lang/Character") ::
            ObjectType ("java/lang/Double") ::
            ObjectType ("java/lang/Float") ::
            ObjectType ("java/lang/Integer") ::
            ObjectType ("java/lang/Long") ::
            ObjectType ("java/lang/Short") ::
            Nil

    def apply(project: Project) = {
        for {classFile ← project.classFiles
             method ← classFile.methods
             if method.body.isDefined
             code = method.body.get
             if code.instructions.exists (_.isInstanceOf[MONITORENTER.type])
             cfg = BaseControlFlow (code)
             df = BaseDataFlow (method, cfg)
             (monitorEnter, idx) ← code.instructions.zipWithIndex.filter (_._1.isInstanceOf[MONITORENTER.type])
             stacks = df (idx).s.collection
             stack ← stacks
             head = stack.get (0)
             if (BAD_SIGNATURES.exists (t => head.getDeclaredType.isOfType (t)) &&
                 !head.isCreatedByNew &&
                 !head.getDeclaredType.isOfType (ObjectType ("java/lang/Boolean"))
                 )
        } yield
        {
            (classFile, method, idx)
        }
    }

}
