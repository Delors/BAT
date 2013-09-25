package de.tud.cs.st.bat.sandbox

import de.tud.cs.st.bat.resolved.analyses.Project
import de.tud.cs.st.bat.resolved._
import scala.Array
import scala.collection.mutable


/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 14.05.13
 * Time: 15:56
 * To change this template use File | Settings | File Templates.
 */
object MyFirstAnalysis
    extends (Project => Iterable[String])
{

    def apply (project: Project) = {


        for {classFile <- project.classFiles
             method <- classFile.methods
             if method.body.isDefined
        } yield
        {
            println ("Methode: " + method)
            VariableMaker.reset
            var locals: mutable.Seq[SSAVariable] = Array.ofDim[SSAVariable](method.body.get.maxLocals)
                .map (x => new SSAVariable (VariableMaker.newVar ()))
            var stack = List.empty[SSAInstruction]
            //var localsState = new Array[Array[SSAVariable]](method.body.get.instructions.size)
            //var stackState = new Array[List[SSAPrimitive]](method.body.get.instructions.size)
            //var lastLocalsState
            //var lastStackState

            var states = new Array[State](method.body.get.instructions.size)
                .map (x => new State ()) // Initialisieren mit null für jeden state

            //var lastStates = Nil // noch benötigt?

            var i = 0
            var res: List[String] = Nil

            var finished: Boolean = false

            states (0) = new State (locals, stack, None)

            while (!finished) {
                //Abbruch, wenn eine Schleife komplett includes korrekt war
                println ("Außen while")
                finished = true
                i = 0


                while (i < method.body.get.instructions.size) {
                    println ("Innen while")
                    /* compute state after instruction*/
                    /* for each next instruciton*/
                    /* mergeNewState(nextInstruction) */

                    val instruction = method.body.get.instructions (i)
                    println (instruction)

                    if (instruction != null) {

                        var result: (Option[SSAInstruction], List[SSAInstruction], mutable.Seq[SSAVariable],
                            Option[List[Int]]) = null
                        //Compute instruction TODO richtige stack und locals nehmen!

                        println ("before i=" + i)
                        println (states (i))
                        result = getApplyInstruction (instruction, states (i).stackState, states (i).localsState, i)
                        stack = result._2
                        locals = result._3

                        val newState = State (locals, stack, result._1)
                        var indices = List.empty[Int]

                        //Herausfinden an welchen Stellen die States gemerged werden müssen
                        if (!result._4.isEmpty) {
                            indices = result._4.get
                        } else
                        {
                            indices = indices.+: (instruction.indexOfNextInstruction (i, method.body.get))
                        }



                        // MERGEN
                        for (j <- indices) {
                            if (j < method.body.get.instructions.size) {
                                val currentState = states (j)
                                val mergeResult = newState.merge (currentState)
                                println ("after i at j= " + j)
                                println (mergeResult)
                                states (j) = mergeResult._1
                                // Für das Abbruchkriterium notwendig; Abbruch, wenn includes jedes mal der Fall war
                                finished = finished && mergeResult._2
                            }
                        }


                    }
                    i = i + 1
                }
            }

            //Ausgabe der SSACommands
            ///*
            for (state <- states) {
                if (state.ssaCommand.isDefined) {
                    val ssaInstr = state.ssaCommand.get
                    val s = MyClient.myMatching (ssaInstr)
                    res = s :: res
                }
            }
            //*/
            /*
            //Testausgabe des Lookuptables
            for(entry <- Memory.lookuptable.iterator){
               val s = MyClient.myMatching(entry._1) + " " + MyClient.myMatching(entry._2)
              res = s :: res

            }
            */
            res.reverse.foldLeft ("")(_ + "\n" + _)
        }
        //getMnemonics(project)
    }

    def getNextInstructionIndex (instructions: Array[Instruction], index: Int): Option[Int] = {
        var j: Int = index
        if (index < instructions.size) {
            if (instructions (index) == null) {
                while (j < instructions.size && instructions (j) == null) {
                    j += 1
                }
            }
            Some (j)
        } else
        {
            None
        }
    }


    def getApplyInstruction (instr: Instruction,
        stack: List[SSAInstruction],
        locals: mutable.Seq[SSAVariable],
        index: Int
    ):
    (Option[SSAInstruction], List[SSAInstruction], mutable.Seq[SSAVariable], Option[List[Int]]) = {
        instr.opcode match {
            case 3 /*ICONST_0*/ => {
                (None, stack.+: (new SSAConstant (0)), locals, None)
            }
            case 4 /*ICONST_1*/ => {
                (None, stack.+: (new SSAConstant (1)), locals, None)
            }
            case 21 /*ILOAD*/ => {

                val dest = instr.asInstanceOf[ILOAD].lvIndex
                (None, stack.+: (locals.apply (dest)), locals, None)
            }
            case 27 /*ILOAD_1*/ => {
                (None, stack.+: (locals.apply (1)), locals, None)
            }
            case 28 /*ILOAD_2*/ => {
                (None, stack.+: (locals.apply (2)), locals, None)
            }
            case 29 /*ILOAD_3*/ => {
                (None, stack.+: (locals.apply (3)), locals, None)
            }
            case 42 /*ALOAD_0*/ => {
                (None, stack.+: (locals (0)), locals, None)
            }
            case 54 /*ISTORE*/ => {
                val dest = instr.asInstanceOf[ISTORE].lvIndex
                val x = iStoring (stack (0))
                locals.update (dest, x)
                //TODO: nötig?
                //Memory.change(x, stack(0))
                (None, stack.tail, locals, None)
            }
            case 60 /*ISTORE_1*/ => {
                val x = iStoring (stack (0))
                locals.update (1, x)
                (None, stack.tail, locals, None)
            }
            case 61 /*ISTORE_2*/ => {
                val x = iStoring (stack (0))
                locals.update (2, x)
                (None, stack.tail, locals, None)
            }
            case 62 /*ISTORE_3*/ => {
                val x = iStoring (stack (0))
                locals.update (3, x)
                (None, stack.tail, locals, None)
            }
            case 96 /*IADD*/ => {
                val x = new SSAVariable (VariableMaker.newVar ())
                val y = new SSAAdd (x, MyClient.varMatching (stack (0)), MyClient.varMatching (stack (1)))
                (Some (y), stack.tail.tail.+: (y), locals, None)
            }
            case 100 /*ISUB*/ => {
                val x = new SSAVariable (VariableMaker.newVar ())
                val y = new SSASub (x, MyClient.varMatching (stack (0)), MyClient.varMatching (stack (1)))
                (Some (y), stack.tail.tail.+: (y), locals, None)
            }
            case 104 /*IMUL*/ => {
                val x = new SSAVariable (VariableMaker.newVar ())
                val y = new SSAMul (x, MyClient.varMatching (stack (0)), MyClient.varMatching (stack (1)))
                (Some (y), stack.tail.tail.+: (y), locals, None)
            }
            case 132 /*IInc*/ => {
                val lvindex = instr.asInstanceOf[IINC].lvIndex
                val constValue = instr.asInstanceOf[IINC].constValue

                val x = locals (lvindex)
                val newX = new SSAVariable (VariableMaker.newVar ())
                val newValue = new SSAAdd (newX, x, SSAConstant (constValue))
                Memory.change (newX, newValue)

                (Some (newValue), stack, locals, None)
            }
            case 161 /*IF_ICMPLT*/ => {
                val offset = instr.asInstanceOf[IF_ICMPLT].branchoffset
                val list = List.empty[Int].:+ (index + offset).:+ (index + 3)
                (None, stack.tail.tail, locals, Some (list))
            }
            case 164 /*IF_ICMPLE*/ => {
                val offset = instr.asInstanceOf[IF_ICMPLE].branchoffset
                val list = List.empty[Int].:+ (index + offset).:+ (index + 3)
                (None, stack.tail.tail, locals, Some (list))
            }
            case 176 /*GOTO*/ => {
                val offset = instr.asInstanceOf[GOTO].branchoffset
                val list = List.empty[Int].:+ (index + offset)
                (None, stack, locals, Some (list))
            }
            case 183 /*Invokespecial*/ => {
                //TODO!
                (None, stack.tail, locals, None)
            }
            case _ => {
                (None, stack, locals, None)
            }
        }
    }


    def getMnemonics (project: Project) = {
        for {classFile <- project.classFiles
             method <- classFile.methods
             if method.body.isDefined
             instruction <- method.body.get.instructions
             if instruction != null
        } yield
        {
            if (instruction.isInstanceOf[GOTO]) {
                instruction.mnemonic + " " + instruction.asInstanceOf[GOTO].branchoffset
            }
            else if (instruction.isInstanceOf[IF_ICMPLE]) {
                instruction.mnemonic + " " + instruction.asInstanceOf[IF_ICMPLE].branchoffset
            }
            else if (instruction.isInstanceOf[IF_ICMPLT]) {
                instruction.mnemonic + " " + instruction.asInstanceOf[IF_ICMPLT].branchoffset
            }
            else
            {
                instruction.mnemonic
            }
        }
    }

    def iStoring (node: SSAInstruction): SSAVariable = {
        var x: SSAVariable = null
        if (node.isInstanceOf[SSAConstant]) {
            x = new SSAVariable (VariableMaker.newVar ())
            Memory.change (x, node)
        } else
        {
            x = MyClient.iStoreMatching (node)
        }
        x
    }


    /*
     def getSSA(instructions: Array[Instruction]) : Set[SSAInstruction] = {
       var i = ..
       val end = new scala.collection.mutable.ArrayBuffer[SSAInstruction]
       while(i < instructions.length){
         if instructions.
         i +=1
       }
     return end

     }              */
}


//case class SSAIfBranch(x:SSAEquation, rop:SSAPrimitve, lop:SSAPrimitive) extends SSAPrimitive

//"SSAAdd(.., ..)"
object MyClient
{

    //val myExpr =  SSAAdd(Variable("x0"), Variable("x1"))

    def myMatching (node: SSAInstruction): String = node match {
        case SSAConstant (v) => v.toString
        case SSAVariable (x) => x.toString
        //case SSAEquation(x, c) => myMatching(x) + " = " + myMatching(c)
        case SSAAdd (va, vb, vc) => myMatching (va) + " = " + myMatching (vb) + " + " + myMatching (vc)
        case SSASub (va, vb, vc) => myMatching (va) + " = " + myMatching (vb) + " - " + myMatching (vc)
        case SSAMul (va, vb, vc) => myMatching (va) + " = " + myMatching (vb) + " * " + myMatching (vc)
        case SSAPhi (va, vb, vc) => myMatching (va) + " = (" + myMatching (vb) + ", " + myMatching (vc) + ")"
        case _ => throw new IllegalArgumentException (node.getClass + " is not supported as argument of varMatching!")
    }

    //TODO noch gebraucht?
    def varMatching (node: SSAInstruction): SSAPrimitive = node match {
        case SSAConstant (v) => node.asInstanceOf[SSAConstant]
        case SSAVariable (x) => node.asInstanceOf[SSAVariable]
        //case SSAEquation(x, c) => varMatching(x)
        case SSAAdd (va, vb, vc) => va
        case SSASub (va, vb, vc) => va
        case SSAMul (va, vb, vc) => va
        case SSAPhi (va, vb, vc) => va
        case _ => throw new IllegalArgumentException (node.getClass + " is not supported as argument of varMatching!")
    }

    def iStoreMatching (node: SSAInstruction): SSAVariable = node match {
        //case SSAConstant(v) => new SSAVariable(VariableMaker.newVar())
        case SSAVariable (x) => node.asInstanceOf[SSAVariable]
        //case SSAEquation(x, c) => varMatching(x)
        case SSAAdd (va, vb, vc) => va
        case SSASub (va, vb, vc) => va
        case SSAMul (va, vb, vc) => va
        case SSAPhi (va, vb, vc) => va
        case _ => throw new IllegalArgumentException (node.getClass + " is not supported as argument of varMatching!")
    }

}


