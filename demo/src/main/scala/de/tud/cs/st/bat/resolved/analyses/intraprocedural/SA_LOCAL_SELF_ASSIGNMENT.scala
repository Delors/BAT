package de.tud.cs.st.bat.resolved.analyses.intraprocedural

import de.tud.cs.st.bat.resolved._
import analyses.Project
import structure._

/**
 * @author Ralf Mitschke
 */
object SA_LOCAL_SELF_ASSIGNMENT
    extends (Project => Iterable[(ClassFile, Method, Int)])
{

    private def isStoreInstruction(instruction: Instruction): Boolean =
        instruction.isInstanceOf[StoreLocalVariableInstruction]


    def apply(project: Project) = {
        for {classFile ← project.classFiles
             method ← classFile.methods
             if method.body.isDefined
             code = method.body.get
             if code.instructions.exists (isStoreInstruction)
             cfg = BaseControlFlow (code)
             df = BaseDataFlow (method, cfg)
             (store, idx) ← code.instructions.zipWithIndex.filter (e => isStoreInstruction (e._1))
             stacks = df (idx).s.collection
             stack ← stacks
             if isSelfStoreInstruction (store, stack, df (idx).l, code.localVariableTable)
        } yield
        {
            (classFile, method, idx)
        }
    }

    private def isSelfStoreInstruction(instr: Instruction, stack: Stack, lv: LocVariables, localVariableTable: Option[LocalVariables]): Boolean = {
        val lvIndex = getIndexOfLocalVariable (instr.asInstanceOf[StoreLocalVariableInstruction])

        //TODO: Remove this test when exceptions are implemented.
        if (stack.size == 0) {

        }
        else if (saveEquals (lv (lvIndex), stack (0))) {
            localVariableTable match {
                case None => return true

                case Some (varTable) => {
                    //TODO: Check if loc variable name is also a field name.
                    if (stack (0).isFromField && stack (0).getFieldName == varTable (lvIndex).name)
                        return false
                    else
                        return true
                }
            }
        }
        false
    }


    private def getIndexOfLocalVariable(instr: StoreLocalVariableInstruction): Int = {
        instr match {
            case ISTORE (x) => x

            case LSTORE (x) => x

            case FSTORE (x) => x

            case DSTORE (x) => x

            case ASTORE (x) => x

            case ISTORE_0 | LSTORE_0 | FSTORE_0 | DSTORE_0 | ASTORE_0 => 0
            case ISTORE_1 | LSTORE_1 | FSTORE_1 | DSTORE_1 | ASTORE_1 => 1
            case ISTORE_2 | LSTORE_2 | FSTORE_2 | DSTORE_2 | ASTORE_2 => 2
            case ISTORE_3 | LSTORE_3 | FSTORE_3 | DSTORE_3 | ASTORE_3 => 3

            case x => throw new IllegalArgumentException (x + ": The store instruction is unknown.")
        }
    }

    private def saveEquals(a: Any, b: Any): Boolean = {
        if (a == null)
            b == null
        else
            a.equals (b)
    }


}
