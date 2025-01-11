package lang

import identifiers.*
import lang.Capturables.*
import lang.CaptureDescriptors.{CaptureDescriptor, CaptureSet, Mark}
import lang.LanguageMode.{OcapDisabled, OcapEnabled}
import lang.Types.PrimitiveTypeShape.{RegionType, VoidType}
import lang.Types.{NamedTypeShape, PrimitiveTypeShape, Type}
import lang.Visibility.Public

import scala.collection.mutable

final case class FunctionSignature(
                                    name: FunOrVarId,
                                    args: List[(Option[FunOrVarId], Type)],
                                    retType: Type,
                                    visibility: Visibility,
                                    languageMode: LanguageMode
                                  ) {

  def argsForMode(requestedMode: LanguageMode): List[(Option[FunOrVarId], Type)] =
    args.map((idOpt, tpe) => (idOpt, convertType(languageMode, requestedMode, tpe)))

  def retTypeForMode(requestedMode: LanguageMode): Type =
    convertType(languageMode, requestedMode, convertType(languageMode, requestedMode, retType))
}

sealed trait TypeSignature {
  def id: TypeIdentifier

  def getNonSubstitutedCaptureDescr: CaptureDescriptor

  def isAbstract: Boolean

  def languageMode: LanguageMode
}

sealed trait FunctionsProviderSig extends TypeSignature {
  def functions: Map[FunOrVarId, FunctionSignature]
}

sealed trait ConstructibleSig extends TypeSignature {

  def params: mutable.LinkedHashMap[FunOrVarId, FieldInfo]
  
  def regularParams: mutable.LinkedHashMap[FunOrVarId, FieldInfo] =
    params.filter((id, _) => id != SpecialFields.regFieldId)

  def globalCaptures: Set[Capturable]

  def voidInitMethodSig: FunctionSignature =
    FunctionSignature(ConstructorFunId, regularParams.toList.map((id, info) => (Some(id), info.tpe)), VoidType,
      Public, languageMode)
}

sealed trait UserConstructibleSig extends TypeSignature {
  this: ConstructibleSig =>
}

sealed trait SelectableSig extends TypeSignature {
  this: ConstructibleSig =>

  def typeOfSelectIfCapturable(sel: FunOrVarId): Option[Type] =
    params.get(sel)
      .filter(!_.isReassignable)
      .map(_.tpe)
}

sealed trait ImporterSig extends TypeSignature {

  def paramImports: mutable.LinkedHashMap[FunOrVarId, Type]

  def importedPackages: mutable.LinkedHashSet[TypeIdentifier]

  def importedDevices: mutable.LinkedHashSet[Device]

  def globalCaptures: Set[Capturable] =
    importedPackages.map(CapPackage(_)).toSet ++ importedDevices.map(CapDevice(_))

  def params: mutable.LinkedHashMap[FunOrVarId, FieldInfo] =
    paramImports.map((id, tpe) => id -> FieldInfo(tpe, isReassignable = false, languageMode))
}

final case class ModuleSignature(
                                  id: TypeIdentifier,
                                  paramImports: mutable.LinkedHashMap[FunOrVarId, Type],
                                  importedPackages: mutable.LinkedHashSet[TypeIdentifier],
                                  importedDevices: mutable.LinkedHashSet[Device],
                                  functions: Map[FunOrVarId, FunctionSignature],
                                  languageMode: LanguageMode
                                )
  extends TypeSignature, ConstructibleSig, UserConstructibleSig, ImporterSig, SelectableSig, FunctionsProviderSig {

  override def getNonSubstitutedCaptureDescr: CaptureDescriptor =
    CaptureSet(globalCaptures ++ paramImports.map((paramId, _) => MePath.dot(paramId)))

  override def isAbstract: Boolean = false
}

final case class PackageSignature(
                                   id: TypeIdentifier,
                                   importedPackages: mutable.LinkedHashSet[TypeIdentifier],
                                   importedDevices: mutable.LinkedHashSet[Device],
                                   functions: Map[FunOrVarId, FunctionSignature],
                                   languageMode: LanguageMode
                                 ) extends TypeSignature, ConstructibleSig, ImporterSig, FunctionsProviderSig {

  override def paramImports: mutable.LinkedHashMap[FunOrVarId, Type] = mutable.LinkedHashMap.empty

  override def getNonSubstitutedCaptureDescr: CaptureDescriptor = CaptureSet(globalCaptures)

  def asType: Type = NamedTypeShape(id) ^ (if languageMode.isOcapEnabled then CaptureSet(globalCaptures) else Mark)

  override def isAbstract: Boolean = false
}

final case class StructSignature(
                                  id: TypeIdentifier,
                                  isShallowMutable: Boolean,
                                  fields: mutable.LinkedHashMap[FunOrVarId, FieldInfo],
                                  directSupertypes: Seq[TypeIdentifier],
                                  directSubtypesOpt: Option[mutable.LinkedHashSet[TypeIdentifier]],
                                  languageMode: LanguageMode
                                )
  extends TypeSignature, ConstructibleSig, UserConstructibleSig, SelectableSig {

  override def isAbstract: Boolean = directSubtypesOpt.isDefined

  override def params: mutable.LinkedHashMap[FunOrVarId, FieldInfo] = fields

  override def getNonSubstitutedCaptureDescr: CaptureDescriptor = CaptureDescriptors.unionOf(
    fields.filter((_, info) => !info.tpe.captureDescriptor.isEmpty)
      .map { (id, info) =>
        if info.isReassignable
        then info.tpe.captureDescriptor
        else CaptureSet(MePath.dot(id))
      }
  )

  override def globalCaptures: Set[Capturable] = Set.empty
}

final case class FieldInfo(tpe: Type, isReassignable: Boolean, languageMode: LanguageMode) {
  def tpeForMode(requestedMode: LanguageMode): Type = convertType(languageMode, requestedMode, tpe)
}

private def convertType(fromMode: LanguageMode, toMode: LanguageMode, tpe: Type): Type = (fromMode, toMode) match {
  case (OcapEnabled, OcapDisabled) => tpe.cdErasedDeep
  case (OcapDisabled, OcapEnabled) => tpe.markedIfNeededDeep
  case _ => tpe
}
