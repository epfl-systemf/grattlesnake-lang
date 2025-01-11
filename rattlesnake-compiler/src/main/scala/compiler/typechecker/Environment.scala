package compiler.typechecker

import compiler.typechecker.SubcaptureRelation.isCoveredBy
import identifiers.FunOrVarId
import lang.Capturables.{ConcreteCapturable, IdPath}
import lang.CaptureDescriptors.CaptureSet

final class Environment(private var allowedCs: CaptureSet, private var allowEverything: Boolean = false) {

  def allow(c: ConcreteCapturable): Unit = {
    allowedCs = allowedCs.augmentedWith(c)
  }

  def allows(c: ConcreteCapturable)(using TypeCheckingContext): Boolean = {
    allowEverything || c.isCoveredBy(allowedCs)
  }

  def copyForSubScope: Environment = new Environment(allowedCs, allowEverything)

  def withEverythingAllowed: Environment = new Environment(allowedCs, allowEverything = true)

  def copyForSubScopeWith(ids: List[FunOrVarId]): Environment = {
    val newEnvir = copyForSubScope
    for id <- ids do {
      newEnvir.allow(IdPath(id))
    }
    newEnvir
  }

}

object Environment {
  
  def root: Environment = new Environment(CaptureSet.empty, allowEverything = true)

}
