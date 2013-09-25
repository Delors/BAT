package de.tud.cs.st.bat.sandbox

import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: Ich
 * Date: 23.07.13
 * Time: 12:22
 * To change this template use File | Settings | File Templates.
 */
case class State(val localsState : mutable.Seq[SSAVariable], val stackState : List[SSAInstruction], val ssaCommand : Option[SSAInstruction]) {

  var _isDummy = false

  def this() =
  {
    this(new Array[SSAVariable](1), List.empty[SSAInstruction], None)
    this._isDummy =true
  }

  def merge(state : State) : (State, Boolean) ={
    //var result : State
    var mergedLocalsState : mutable.Seq[SSAVariable] = Array.empty[SSAVariable]
    var mergedStackState  : List[SSAInstruction] = List.empty[SSAInstruction]
    var mergedSSACommand : Option[SSAInstruction] = None
    var includeSucceeded : Boolean = true

    // MERGE LOCALS
    if (!state.isDummy()){
      if(!areLocalsEqual(localsState, state.localsState)){
        //TODO size sicher machen? Bisher Annahme beide Locals gleiche Länge
        var i = 0
        while(i < localsState.size){
          val ssaVarNew  = localsState(i)
          val ssaVarOld = state.localsState(i) //Umbenennen??? Langsam unübersichtlich

          if(ssaVarNew.includes(ssaVarOld)){ // Information ssaVarOld in ssaVarNew enthalten
            Memory.change(ssaVarOld, Memory.getValue(ssaVarNew)) //Überschreiben der alten Variablen mit dem neuen, das Alte beinhaltende Inhalt
            mergedLocalsState(i) = ssaVarOld  //Alte Variable mit (eventuell) neuem Inhalt im merged; Inhalt in lookuptable sichtbar
          } else {
            // ssaVarNew enthält nicht ssaVarOld
            val newX = new SSAVariable(VariableMaker.newVar())
            val newPhi = new SSAPhi(newX, ssaVarNew, ssaVarOld)
            mergedLocalsState(i) =  newX
            includeSucceeded = false
          }

          i += 1
        }

      } else{
        mergedLocalsState = localsState
        println("Locals equal!")
        //_isDummy = false
        //includeSucceeded = true
      }
    } else{
      mergedLocalsState=localsState
      includeSucceeded = false
    }


    // MERGE STACK

    if(!state.isDummy()){
      if(!(areStacksEqual(stackState, state.stackState))){
        if(stackState.size == state.stackState.size){
        //TODO size sicher machen? Bisher Annahme beide Stacks gleiche Länge
          var i = 0
          while(i < stackState.size){
            val ssaInstrNew = stackState(i)
            val ssaInstrOld = state.stackState(i)

            //Information ssaInstrOld in ssaInstrNew enthalten, ggf Überschreiben von Inhalt ssaInstrOld mit ssaInstrNew
            if(ssaInstrNew.includes(ssaInstrOld) ){
              println("ssaInstrNew.includes(ssaInstrOld)")
              if(ssaInstrOld.isInstanceOf[SSAEquation] && ssaInstrNew.isInstanceOf[SSAEquation] ){
                val oldEQ = ssaInstrOld.asInstanceOf[SSAEquation]
                val newEQ = ssaInstrNew.asInstanceOf[SSAEquation]
                val updatedEQ = changeContentSSAEquation(newEQ, oldEQ)
                Memory.change(updatedEQ.y, updatedEQ)
                mergedStackState = mergedStackState.:+(updatedEQ)
                println("SSAEQUATION MERGE!")
              } else{
                mergedStackState = mergedStackState.:+(ssaInstrNew)
                println("SSAEQUATION ELSE MERGE!")
                //includeSucceeded = false ???
              }
            } else{
              println("!ssaInstrNew.includes(ssaInstrOld)")
              val newX = new SSAVariable(VariableMaker.newVar())
              val newPhi = new SSAPhi(newX, ssaInstrNew, ssaInstrOld)
              mergedStackState = mergedStackState.:+(newPhi)
              includeSucceeded = false //false
            }

            i += 1
          }
        } else {
          mergedStackState = state.stackState //TODO experimentell alten Stack behalten, wenn Längen zwischen neu und alt sich unterscheiden
        }
      }else{
        println("Stacks equal")
        mergedStackState=stackState
        //includeSucceeded = true
      }
    }else{
      mergedStackState = stackState
      //_isDummy = false
      includeSucceeded = false
    }

    // MERGE SSACOMMAND

      if(ssaCommand.isDefined){
        if(state.ssaCommand.isDefined){
          if(ssaCommand.get.includes(state.ssaCommand.get)){
            if(state.ssaCommand.get.isInstanceOf[SSAEquation] && ssaCommand.get.isInstanceOf[SSAEquation] ){
              val oldEQ = state.ssaCommand.get.asInstanceOf[SSAEquation]
              val newEQ = ssaCommand.get.asInstanceOf[SSAEquation]
              val updatedEQ = changeContentSSAEquation(newEQ, oldEQ)
              Memory.change(updatedEQ.y, updatedEQ)
              mergedSSACommand = Some(updatedEQ)
              println("SSAEQUATION MERGE!")
            } else{
              mergedSSACommand = ssaCommand
            }
          }

        } else{
          mergedSSACommand = ssaCommand
        }
      } else{
        mergedSSACommand = None
      }

    (new State(mergedLocalsState, mergedStackState, mergedSSACommand), includeSucceeded)
  }

  def changeContentSSAEquation(ssaEqNew : SSAEquation, ssaEqOld : SSAEquation) : SSAEquation = ssaEqNew match{
    case SSAAdd(va, vb, vc) => SSAAdd(ssaEqOld.y, ssaEqNew.fi, ssaEqNew.si)
    case SSASub(va, vb, vc) => SSASub(ssaEqOld.y, ssaEqNew.fi, ssaEqNew.si)
    case SSAMul(va, vb, vc) => SSAMul(ssaEqOld.y, ssaEqNew.fi, ssaEqNew.si)
    case SSAPhi(va, vb, vc) => SSAPhi(ssaEqOld.y, ssaEqNew.fi, ssaEqNew.si)
  }

  def areStacksEqual(stack1 : List[SSAInstruction], stack2 : List[SSAInstruction]) : Boolean ={

    if(stack1.size != stack2.size){
      return false
    }

    var i = 0
    while(i < stack1.size){
      if(!stack1(i).isEqual(stack2(i))){
        return false
      }
      i += 1
    }

    return true
  }

  def areLocalsEqual(locals1 : mutable.Seq[SSAVariable], locals2 : mutable.Seq[SSAVariable]) : Boolean ={

    if(locals1.size != locals2.size){
      return false
    }

    var i = 0
    while(i < locals1.size){
      if(!locals1(i).isEqual(locals2(i))){
        return false
      }
      i += 1
    }
    return true
  }

  def isEqual(state : State) : Boolean ={
     if (ssaCommand.isDefined && state.ssaCommand.isDefined){
      (localsState == state.localsState && stackState == state.stackState && ssaCommand.get.isEqual(state.ssaCommand.get))
     }else {
      (localsState == state.localsState && stackState == state.stackState)
      }
  }

  def isDummy() : Boolean ={
    _isDummy
  }

}
