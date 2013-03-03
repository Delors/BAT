package de.tud.cs.st.bat.resolved.analyses.intraprocedural

import de.tud.cs.st.bat.resolved._
import analyses.Project
import structure.Stack


/**
 * @author Ralf Mitschke
 */
object SA_FIELD_SELF_COMPARISON
    extends (Project => Iterable[(ClassFile, Method, Int)])
{


    private def isBranchInstruction(instr: Instruction): Boolean = {
        instr.isInstanceOf[ConditionalBranchInstruction] &&
            !instr.isInstanceOf[IFNE] &&
            !instr.isInstanceOf[IFEQ] &&
            !instr.isInstanceOf[IFGT] &&
            !instr.isInstanceOf[IFGE] &&
            !instr.isInstanceOf[IFLE] &&
            !instr.isInstanceOf[IFLT] &&
            !instr.isInstanceOf[IFNONNULL] &&
            !instr.isInstanceOf[IFNULL]
    }

    //Comparison using compareTo or equals
    private def isCompareCall(instr: Instruction): Boolean = {
        if (instr.isInstanceOf[INVOKEVIRTUAL]) {
            val invoke = instr.asInstanceOf[INVOKEVIRTUAL]
            return (invoke.name == "equals" && invoke.methodDescriptor.parameterTypes.size == 1 && invoke.methodDescriptor.returnType.isInstanceOf[BooleanType]) ||
                (invoke.name == "compareTo" && invoke.methodDescriptor.parameterTypes.size == 1 && invoke.methodDescriptor.returnType.isInstanceOf[IntegerType])
        }

        if (instr.isInstanceOf[INVOKEINTERFACE]) {
            val invoke = instr.asInstanceOf[INVOKEINTERFACE]
            return invoke.name == "compareTo" && invoke.methodDescriptor.parameterTypes.size == 1 && invoke.methodDescriptor.returnType.isInstanceOf[IntegerType]
        }

        false
    }

    private def makesComparison(instruction: Instruction): Boolean =
        isBranchInstruction (instruction) || isCompareCall (instruction)


    def apply(project: Project) = {
        for {classFile ← project.classFiles
             method ← classFile.methods
             if method.body.isDefined
             code = method.body.get
             //if code.instructions.exists (makesComparison)
             cfg = BaseControlFlow (code)
             df = BaseDataFlow (method, cfg)
             (invoke, idx) ← code.instructions.zipWithIndex.filter (e => makesComparison (e._1))
             stacks = df (idx).s.collection
             stack ← stacks
             if isSelfComparison (stack, code.instructions)
        } yield
        {
            (classFile, method, idx)
        }
    }


    private def isSelfComparison(stack: Stack, instructions: Array[Instruction]): Boolean = {
        if (stack.size < 2)
            return false

        val rhs = stack.get (0)
        val lhs = stack.get (1)

        if (rhs.getPC != -1 && lhs.getPC != -1) {

            val rInstr = instructions (rhs.getPC)
            val lInstr = instructions (lhs.getPC)

            if ((rInstr.isInstanceOf[GETFIELD] && lInstr.isInstanceOf[GETFIELD])
                || (instructions (rhs.getPC).isInstanceOf[GETSTATIC] && instructions (lhs.getPC).isInstanceOf[GETSTATIC]))
            {
                if (rInstr.equals (lInstr)) {
                    return true
                }
            }
        }

        false
    }

}
