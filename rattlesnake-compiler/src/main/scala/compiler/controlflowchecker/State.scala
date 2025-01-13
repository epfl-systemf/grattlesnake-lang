package compiler.controlflowchecker

import compiler.controlflowchecker.InitializationStatus.{Initialized, Uninitialized}
import compiler.irs.Asts.VariableRef
import compiler.pipeline.CompilationStep.PathsChecking
import compiler.reporting.Errors.{Err, ErrorReporter}
import compiler.reporting.Position
import identifiers.{FunOrVarId, TypeIdentifier}
import lang.Types.NamedTypeShape


final case class State(
                        alwaysTerminated: Boolean,
                        locals: Map[FunOrVarId, InitializationStatus],
                        alreadyCoveredTypesOfVals: Map[FunOrVarId, Set[TypeIdentifier]]
                      ) {

  def checkIsInitialized(varRef: VariableRef, er: ErrorReporter): Unit = {
    locals.get(varRef.name).foreach {
      case InitializationStatus.Initialized => ()
      case InitializationStatus.Uninitialized =>
        er.push(Err(PathsChecking, s"'${varRef.name}' has not been initialized", varRef.getPosition))
      case InitializationStatus.PossiblyUninitialized =>
        er.push(Err(PathsChecking, s"cannot prove that '${varRef.name}' has been initialized", varRef.getPosition))
    }
  }

  def checkIsNotInitialized(varRef: VariableRef, er: ErrorReporter): Unit = {
    locals.get(varRef.name).foreach {
      case InitializationStatus.Initialized =>
        er.push(Err(PathsChecking, s"${varRef.name} cannot be reassigned", varRef.getPosition))
      case InitializationStatus.Uninitialized => ()
      case InitializationStatus.PossiblyUninitialized =>
        er.push(Err(PathsChecking, s"cannot prove that ${varRef.name} has not already been initialized", varRef.getPosition))
    }
  }

  /**
   * @return true iff reachable
   */
  def checkIsReachable(er: ErrorReporter, posOpt: Option[Position]): Boolean = {
    if (alwaysTerminated) {
      er.push(Err(PathsChecking, "unreachable code", posOpt))
    }
    !alwaysTerminated
  }

  def terminated(): State = copy(alwaysTerminated = true)

  def newLocalSaved(localId: FunOrVarId, initialized: Boolean): State = {
    val initializationStatus = if initialized then Initialized else Uninitialized
    copy(locals = locals.updated(localId, initializationStatus))
  }

  def assignmentSaved(localId: FunOrVarId): State = {
    copy(locals = locals.updated(localId, Initialized))
  }

  def handledCaseSaved(valId: FunOrVarId, tpe: TypeIdentifier): State = {
    copy(alreadyCoveredTypesOfVals = alreadyCoveredTypesOfVals.updatedWith(valId) {
      case None => Some(Set(tpe))
      case Some(old) => Some(old + tpe)
    })
  }

  def isUnfeasible(ctx: ControlFlowCheckingContext): Boolean = {
    val iter = alreadyCoveredTypesOfVals.iterator
    while (iter.hasNext) {
      val (valId, coveredTypes) = iter.next()
      ctx.typeOf(valId).shape match {
        case NamedTypeShape(totalTypeId)
          if !ctx.isReassignable(valId) && ctx.analysisContext.datatypeIsCovered(totalTypeId, coveredTypes) =>
          return true
        case _ => ()
      }
    }
    false
  }

  def joined(that: State): State = State(
    this.alwaysTerminated && that.alwaysTerminated,
    if this.alwaysTerminated then that.locals
    else if that.alwaysTerminated then this.locals
    else mergeLocals(that),
    if this.alwaysTerminated then that.alreadyCoveredTypesOfVals
    else if that.alwaysTerminated then this.alreadyCoveredTypesOfVals
    // The most precise thing would be to combine the already covered types (T is covered if it is covered in both sides).
    // This is not done here for simplicity, hence an empty map is returned.
    else Map.empty
  )

  private def mergeLocals(that: State) = {
    (for (localId <- (this.locals.keys ++ that.locals.keys))
      yield localId -> joined(
        this.locals.get(localId),
        that.locals.get(localId)
      )).toMap
  }

  private def joined(lStatusOpt: Option[InitializationStatus], rStatusOpt: Option[InitializationStatus]): InitializationStatus = {
    (lStatusOpt, rStatusOpt) match {
      case (Some(s), None) => s
      case (None, Some(s)) => s
      case (Some(s1), Some(s2)) => s1.joined(s2)
      case _ => throw new AssertionError("should not happen")
    }
  }

}

object State {

  def initial: State = State(false, Map.empty, Map.empty)

}
