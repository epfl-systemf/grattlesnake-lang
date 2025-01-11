package compiler.typechecker

import compiler.analysisctx.AnalysisContext
import compiler.pipeline.CompilationStep.TypeChecking
import compiler.reporting.Errors.{ErrorReporter, Warning}
import compiler.reporting.{Errors, Position}
import compiler.typechecker.TypeCheckingContext.{LocalInfo, LocalUsesCollector}
import identifiers.{FunOrVarId, SpecialFields, TypeIdentifier}
import lang.*
import lang.Capturables.{ConcreteCapturable, IdPath, Path, RootCapability}
import lang.CaptureDescriptors.*
import lang.Types.*
import lang.Types.PrimitiveTypeShape.{NothingType, RegionType, VoidType}

import scala.collection.mutable

/**
 * Mutable context for type checking
 */
final case class TypeCheckingContext private(
                                              analysisContext: AnalysisContext,
                                              private val locals: mutable.Map[FunOrVarId, LocalInfo] = mutable.Map.empty,
                                              meTypeId: TypeIdentifier,
                                              meCaptureDescr: CaptureDescriptor,
                                              currentFunIdOpt: Option[FunOrVarId],
                                              insideRegionsScope: Boolean,
                                              insideEnclosure: Boolean,
                                              currentRestriction: CaptureSet
                                            ) {

  def meType: Type = NamedTypeShape(meTypeId) ^ meCaptureDescr

  def currentModuleIsPackage: Boolean = resolveTypeAs[PackageSignature](meTypeId).isDefined

  // Locals that have been created by this context (i.e. not obtained via a copy)
  private val ownedLocals: mutable.Set[FunOrVarId] = mutable.Set.empty

  def copyForSubScope: TypeCheckingContext = copy(locals = mutable.Map.from(locals))

  def copyWithSmartCasts(smartCasts: Map[FunOrVarId, TypeShape]): TypeCheckingContext = {
    copy(locals = locals.map {
      case (id, info) => id -> info.copy(tpe = smartCasts.getOrElse(id, info.tpe))
    })
  }
  
  def copyForRegionScope: TypeCheckingContext = copy(insideRegionsScope = true)
  
  def copyWithRestriction(restr: CaptureSet): TypeCheckingContext = copy(currentRestriction = restr)
  
  def copyForEnclosure: TypeCheckingContext = copy(insideEnclosure = true)

  /**
   * Register a new local
   *
   * @param name                  name of the local
   * @param tpe                   type of the local
   * @param isReassignable        whether it is allowed or not to assign a new value to this local (`val` vs `var`)
   * @param duplicateVarCallback  to be called if the name is already used by another local
   * @param forbiddenTypeCallback to be called if the local has a type that is not acceptable for a local
   */
  def addLocal(
                name: FunOrVarId,
                tpe: Type,
                defPos: Option[Position],
                isReassignable: Boolean,
                declHasTypeAnnot: Boolean,
                duplicateVarCallback: () => Unit,
                forbiddenTypeCallback: () => Unit
              ): Unit = {
    if (tpe == NothingType || tpe == VoidType) {
      forbiddenTypeCallback()
    } else if (locals.contains(name)) {
      duplicateVarCallback()
    } else {
      locals.put(name, LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot))
      ownedLocals.addOne(name)
    }
  }

  def getLocalOnly(name: FunOrVarId): Option[LocalInfo] = locals.get(name)

  def getLocalOrConst(name: FunOrVarId): Option[LocalInfo] = {
    getLocalOnly(name).orElse(
      analysisContext.constants
        .get(name)
        // defPos and declHasTypeAnnot are never used for constants, as long as constants can only be of primitive types
        .map(tpe => LocalInfo(name, tpe, isReassignable = false, defPos = None, declHasTypeAnnot = false))
    )
  }

  def isUninhabitedForSureWhenNoCaptureDescr(shape: TypeShape): Boolean = shape match {
    case RegionType => true
    case NamedTypeShape(typeName) =>
      resolveTypeAs[StructSignature](typeName) match {
        case Some(structSig) => structSig.isShallowMutable
        case None => false
      }
    case _ => false
  }

  def neverNeedsCapDescr(shape: TypeShape): Boolean = shape match {
    case RegionType => false
    case shape: PrimitiveTypeShape => true
    case _ => false
  }

  def lookup(path: Path): Type = path match {
    case Capturables.IdPath(id) =>
      getLocalOnly(id).filter(!_.isReassignable).map(_.tpe)
        .getOrElse(UndefinedTypeShape)
    case Capturables.MePath => meType
    case Capturables.SelectPath(directRoot, fld) =>
      lookup(directRoot).shape match {
        case NamedTypeShape(typeName) =>
          resolveTypeAs[SelectableSig](typeName).flatMap(_.typeOfSelectIfCapturable(fld))
            .getOrElse(UndefinedTypeShape)
        case ArrayTypeShape(elemType) if fld == SpecialFields.regFieldId =>
          RegionType ^ CaptureSet.singletonOfRoot
        case _ => UndefinedTypeShape
      }
  }

  def lookup(capturable: ConcreteCapturable): Type = capturable match {
    case path: Path => lookup(path)
    case Capturables.CapPackage(pkgName) =>
      packages.get(pkgName).map(_.asType)
        .getOrElse(UndefinedTypeShape)
    case Capturables.CapDevice(device) =>
      device.tpe
  }

  def isProper(tpe: Type): Boolean = isProper(tpe.captureDescriptor)

  def isProper(cd: CaptureDescriptor): Boolean = cd match {
    case Mark => false
    case CaptureSet(set) => set.forall {
      case RootCapability => true
      case concr: ConcreteCapturable =>
        val tpe = lookup(concr)
        isProper(tpe)
    }
  }

  def isCurrentFunc(owner: TypeIdentifier, funId: FunOrVarId): Boolean = {
    owner == meTypeId && currentFunIdOpt.contains(funId)
  }
  
  def returnIsPermitted: Boolean = !(insideRegionsScope || insideEnclosure)

  def localIsQueried(localId: FunOrVarId): Unit = {
    locals.get(localId).foreach { l =>
      l.usesCollector.queried = true
    }
  }

  def varIsAssigned(varId: FunOrVarId): Unit = {
    locals.get(varId).foreach { l =>
      l.usesCollector.reassigned = true
    }
  }

  def writeLocalsRelatedWarnings(errorReporter: ErrorReporter): Unit = {
    for (_, LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot, usesCollector)) <- locals if ownedLocals.contains(name) do {
      if (!usesCollector.queried) {
        errorReporter.push(Warning(TypeChecking, s"unused local: '$name' is never queried", defPos))
      } else if (isReassignable && !usesCollector.reassigned) {
        errorReporter.push(Warning(TypeChecking, s"value declared as variable: '$name' could be a ${Keyword.Val}", defPos))
      }
    }
  }

  export analysisContext.*

}

object TypeCheckingContext {

  def apply(
             analysisContext: AnalysisContext,
             meTypeId: TypeIdentifier,
             meCaptureDescr: CaptureDescriptor,
             currFunIdOpt: Option[FunOrVarId],
             insideRegionsScope: Boolean,
             insideEnclosure: Boolean,
             currentRestriction: CaptureSet
           ): TypeCheckingContext = {
    new TypeCheckingContext(analysisContext, mutable.Map.empty, meTypeId,
      meCaptureDescr, currFunIdOpt, insideRegionsScope,
      insideEnclosure, currentRestriction)
  }

  final case class LocalInfo private(
                                      name: FunOrVarId,
                                      tpe: Type,
                                      isReassignable: Boolean,
                                      defPos: Option[Position],
                                      declHasTypeAnnot: Boolean,
                                      usesCollector: LocalUsesCollector
                                    ) {
    def copy(
              name: FunOrVarId = name,
              tpe: Type = tpe,
              isReassignable: Boolean = isReassignable,
              defPos: Option[Position] = defPos,
              declHasTypeAnnot: Boolean = declHasTypeAnnot,
              usesCollector: LocalUsesCollector = usesCollector
            ): LocalInfo = {
      LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot, usesCollector)
    }
  }

  object LocalInfo {
    def apply(
               name: FunOrVarId,
               tpe: Type,
               isReassignable: Boolean,
               defPos: Option[Position],
               declHasTypeAnnot: Boolean
             ): LocalInfo = {
      new LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot, new LocalUsesCollector())
    }
  }

  final class LocalUsesCollector {
    var queried = false
    var reassigned = false
  }

}
