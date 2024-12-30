package compiler.controlflowchecker

import compiler.analysisctx.AnalysisContext
import identifiers.FunOrVarId
import lang.Types.Type

import scala.collection.mutable

/**
 * @param localsAndConsts localId -> (isReassignable, type)
 */
final class ControlFlowCheckingContext private(
                                          val analysisContext: AnalysisContext,
                                          localsAndConsts: mutable.Map[FunOrVarId, (Boolean, Type)]
                                        ) {

  def copyForNarrowedScope(): ControlFlowCheckingContext =
    ControlFlowCheckingContext(analysisContext, mutable.Map.from(localsAndConsts))

  def saveLocal(id: FunOrVarId, isReassignable: Boolean, tpe: Type): Unit = {
    localsAndConsts.put(id, (isReassignable, tpe))
  }

  def isReassignable(id: FunOrVarId): Boolean = localsAndConsts.apply(id)._1

  def typeOf(id: FunOrVarId): Type = localsAndConsts.apply(id)._2

  def unknownVarsRemoved(state: State): State = State(
    state.alwaysTerminated,
    state.locals.filter((id, _) => localsAndConsts.contains(id)),
    state.possibleTypesOfVals.filter((id, _) => localsAndConsts.contains(id))
  )

}

object ControlFlowCheckingContext {
  def empty(analysisContext: AnalysisContext): ControlFlowCheckingContext = ControlFlowCheckingContext(
    analysisContext,
    mutable.Map.empty
  )
}
