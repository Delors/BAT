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
package de.tud.cs.st.bat.resolved.analyses

import interprocedural._
import intraprocedural._
import metrics._
import selected._
import random._

/**
 *
 * @author Ralf Mitschke
 *
 */

object Analyses
{

    def apply(analysisName: String): Project => Iterable[_] = analysisName match {
        case "CI_CONFUSED_INHERITANCE" => CI_CONFUSED_INHERITANCE
        case "CN_IDIOM" => CN_IDIOM
        case "CN_IDIOM_NO_SUPER_CALL" => CN_IDIOM_NO_SUPER_CALL
        case "CN_IMPLEMENTS_CLONE_BUT_NOT_CLONEABLE" => CN_IMPLEMENTS_CLONE_BUT_NOT_CLONEABLE
        case "CO_ABSTRACT_SELF" => CO_ABSTRACT_SELF
        case "CO_SELF_NO_OBJECT" => CO_SELF_NO_OBJECT
        case "DM_GC" => DM_GC
        case "DM_RUN_FINALIZERS_ON_EXIT" => DM_RUN_FINALIZERS_ON_EXIT
        case "EQ_ABSTRACT_SELF" => EQ_ABSTRACT_SELF
        case "FI_PUBLIC_SHOULD_BE_PROTECTED" => FI_PUBLIC_SHOULD_BE_PROTECTED
        case "IMSE_DONT_CATCH_IMSE" => IMSE_DONT_CATCH_IMSE
        case "SE_NO_SUITABLE_CONSTRUCTOR" => SE_NO_SUITABLE_CONSTRUCTOR
        case "UUF_UNUSED_FIELD" => UUF_UNUSED_FIELD

        case "BX_BOXING_IMMEDIATELY_UNBOXED_TO_PERFORM_COERCION" => BX_BOXING_IMMEDIATELY_UNBOXED_TO_PERFORM_COERCION
        case "DMI_LONG_BITS_TO_DOUBLE_INVOKED_ON_INT" => DMI_LONG_BITS_TO_DOUBLE_INVOKED_ON_INT
        case "DP_DO_INSIDE_DO_PRIVILEGED" => DP_DO_INSIDE_DO_PRIVILEGED
        case "FI_USELESS" => FI_USELESS
        case "ITA_INEFFICIENT_TO_ARRAY" => ITA_INEFFICIENT_TO_ARRAY
        case "MS_PKGPROTECT" => MS_PKGPROTECT
        case "MS_SHOULD_BE_FINAL" => MS_SHOULD_BE_FINAL
        case "SE_BAD_FIELD_INNER_CLASS" => SE_BAD_FIELD_INNER_CLASS
        case "SIC_INNER_SHOULD_BE_STATIC_ANON" => SIC_INNER_SHOULD_BE_STATIC_ANON
        case "SW_SWING_METHODS_INVOKED_IN_SWING_THREAD" => SW_SWING_METHODS_INVOKED_IN_SWING_THREAD
        case "UG_SYNC_SET_UNSYNC_GET" => UG_SYNC_SET_UNSYNC_GET
        case "UR_UNINIT_READ_CALLED_FROM_SUPER_CONSTRUCTOR" => UR_UNINIT_READ_CALLED_FROM_SUPER_CONSTRUCTOR


        /* metrics */
        case "CA" => CA
        case "CE" => CE
        case "DIT" => DIT
        case "LCOM" => LCOM

        /* intra-procedural bugfinders */

        case "DL_SYNCHRONIZATION" => DL_SYNCHRONIZATION
        case "DMI_INVOKING_TOSTRING_ON_ARRAY" => DMI_INVOKING_TOSTRING_ON_ARRAY
        case "RC_REF_COMPARISON" => RC_REF_COMPARISON
        case "RV_RETURN_VALUE_IGNORED" => RV_RETURN_VALUE_IGNORED
        case "SA_FIELD_SELF_COMPARISON" => SA_FIELD_SELF_COMPARISON
        case "SA_LOCAL_SELF_ASSIGNMENT" => SA_LOCAL_SELF_ASSIGNMENT
        case "SQL_BAD_PREPARED_STATEMENT_ACCESS" => SQL_BAD_PREPARED_STATEMENT_ACCESS

        /* inter-procedural */
        case "CHA" => CHA
        case "RTA" => RTA

        case _ => throw new IllegalArgumentException ("Unknown analysis: " + analysisName)
    }

}
