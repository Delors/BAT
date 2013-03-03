package de.tud.cs.st.bat.resolved.analyses.intraprocedural

import de.tud.cs.st.bat.resolved._
import analyses.Project
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


    private def callsPreparedStatementSet(instruction: Instruction): Boolean = {
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
             if method.body.isDefined
             code = method.body.get
             //if code.instructions.exists (callsPreparedStatementSet)
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
    /*

   // Fixed list of suffixes for get/set/update methods
   private static final Set<String> dbFieldTypesSet = new HashSet<String>() {
       static final long serialVersionUID = -3510636899394546735L;
       {
           add("Array");
           add("AsciiStream");
           add("BigDecimal");
           add("BinaryStream");
           add("Blob");
           add("Boolean");
           add("Byte");
           add("Bytes");
           add("CharacterStream");
           add("Clob");
           add("Date");
           add("Double");
           add("Float");
           add("Int");
           add("Long");
           add("Object");
           add("Ref");
           add("RowId");
           add("Short");
           add("String");
           add("Time");
           add("Timestamp");
           add("UnicodeStream");
           add("URL");
       }
   };

   //RM: Bug reported on seeing invoke_interface
   public void sawOpcode(int seen) {

       if (seen == INVOKEINTERFACE) {
           String methodName = getNameConstantOperand();
           String clsConstant = getClassConstantOperand();
           if ((clsConstant.equals("java/sql/ResultSet") && ((methodName.startsWith("get") && dbFieldTypesSet
                   .contains(methodName.substring(3))) || (methodName.startsWith("update") && dbFieldTypesSet
                   .contains(methodName.substring(6)))))
                   || ((clsConstant.equals("java/sql/PreparedStatement") && ((methodName.startsWith("set") && dbFieldTypesSet
                           .contains(methodName.substring(3))))))) {
               String signature = getSigConstantOperand();
               int numParms = PreorderVisitor.getNumberArguments(signature);
               if (stack.getStackDepth() >= numParms) {
                   OpcodeStack.Item item = stack.getStackItem(numParms - 1);

                   if ("I".equals(item.getSignature()) && item.couldBeZero()) {
                       bugReporter.reportBug(new BugInstance(this,
                               clsConstant.equals("java/sql/PreparedStatement") ? "SQL_BAD_PREPARED_STATEMENT_ACCESS"
                                       : "SQL_BAD_RESULTSET_ACCESS", item.mustBeZero() ? HIGH_PRIORITY : NORMAL_PRIORITY)
                               .addClassAndMethod(this).addSourceLine(this));
                   }
               }
           }
       }

   }
    */
}
