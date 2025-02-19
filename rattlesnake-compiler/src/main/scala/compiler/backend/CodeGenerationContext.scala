package compiler.backend

import compiler.analysisctx.AnalysisContext
import compiler.backend.CodeGenerationContext.from
import compiler.backend.TypesConverter.numSlotsFor
import identifiers.{FunOrVarId, TypeIdentifier}
import lang.Types.PrimitiveTypeShape.*
import lang.Types.{PrimitiveTypeShape, Type, TypeShape}
import org.objectweb.asm.Label

import scala.annotation.tailrec
import scala.collection.mutable

final class CodeGenerationContext(
                                   val analysisContext: AnalysisContext,
                                   locals: List[mutable.Map[FunOrVarId, (Type, Int)]],
                                   var currLocalIdx: Int,
                                   val currentModule: TypeIdentifier,
                                   val functionStartLabel: Label
                                 ) {

  /**
   * @return a copy of this with a new empty local frame added on top of it
   */
  def withNewLocalsFrame: CodeGenerationContext = {
    new CodeGenerationContext(
      analysisContext,
      mutable.Map.empty[FunOrVarId, (Type, Int)] :: locals,
      currLocalIdx,
      currentModule,
      functionStartLabel
    )
  }

  /**
   * Register a new local
   */
  def addLocal(name: FunOrVarId, tpe: Type): Unit = {
    locals.head.put(name, (tpe, currLocalIdx))
    currLocalIdx += numSlotsFor(tpe.shape)
  }

  def getLocal(name: FunOrVarId): Option[(Type, Int)] = {

    @tailrec def searchLocal(remFrames: List[mutable.Map[FunOrVarId, (Type, Int)]]): Option[(Type, Int)] = {
      remFrames match {
        case Nil => None
        case head :: tail =>
          head.get(name) match {
            case None => searchLocal(tail)
            case some => some
          }
      }
    }

    searchLocal(locals)
  }

  export analysisContext.*

}

object CodeGenerationContext {

  def from(analysisContext: AnalysisContext, currentModule: TypeIdentifier, functionStartLabel: Label): CodeGenerationContext = {
    new CodeGenerationContext(analysisContext, List(mutable.Map.empty[FunOrVarId, (Type, Int)]), 0,
      currentModule, functionStartLabel)
  }

}
