package compiler.analysisctx

import compiler.analysisctx.AnalysisContext.{FunctionFound, FunctionNotFound, MethodResolutionResult, ModuleNotFound}
import identifiers.{FunOrVarId, TypeIdentifier}
import lang.*
import lang.Types.*

import scala.collection.mutable
import scala.reflect.ClassTag

final case class AnalysisContext private[analysisctx](
                                                       modules: Map[TypeIdentifier, ModuleSignature],
                                                       packages: Map[TypeIdentifier, PackageSignature],
                                                       structs: Map[TypeIdentifier, StructSignature],
                                                       constants: Map[FunOrVarId, Type]
                                                     ) {

  def knowsUserDefType(tid: TypeIdentifier): Boolean =
    knowsPackageOrModule(tid) || knowsStruct(tid)

  def knowsPackageOrModule(tid: TypeIdentifier): Boolean = knowsPackage(tid) || knowsModule(tid)

  def knowsPackage(tid: TypeIdentifier): Boolean = packages.contains(tid)

  def knowsModule(tid: TypeIdentifier): Boolean = modules.contains(tid)

  def knowsStruct(tid: TypeIdentifier): Boolean = structs.contains(tid)

  def resolveType(tid: TypeIdentifier): Option[TypeSignature] =
    structs.get(tid)
      .orElse(modules.get(tid))
      .orElse(packages.get(tid))

  def resolveTypeAs[S <: TypeSignature : ClassTag](tid: TypeIdentifier): Option[S] = resolveType(tid) match {
    case Some(s: S) => Some(s)
    case _ => None
  }

  def resolveFunc(owner: TypeIdentifier, methodName: FunOrVarId): MethodResolutionResult = {
    resolveTypeAs[FunctionsProviderSig](owner)
      .map { ownerSig =>
        ownerSig.functions.get(methodName) map { sig =>
          FunctionFound(ownerSig, sig)
        } getOrElse FunctionNotFound(ownerSig)
      } getOrElse ModuleNotFound
  }

  def interfaceIsCovered(totalStructId: TypeIdentifier, possiblyCoveringSubstructs: Set[TypeIdentifier]): Boolean = {
    val worklist = mutable.Queue.empty[TypeIdentifier]
    worklist.enqueue(totalStructId)
    while (worklist.nonEmpty) {
      val currTypeId = worklist.dequeue()
      if (!possiblyCoveringSubstructs.contains(currTypeId)) {
        resolveTypeAs[StructSignature](currTypeId) match {
          case None =>
            // happens if totalStructId is not the id of a struct
            return false
          case Some(sig) =>
            sig.directSubtypesOpt match {
              case None =>
                // happens if we have reached a leaf of the substructuring tree that is not covered
                return false
              case Some(directSubtypes) =>
                worklist.enqueueAll(directSubtypes)
            }
        }
      }
    }
    true
  }

}

object AnalysisContext {

  sealed trait MethodResolutionResult {
    def getFunSigOrThrow(): FunctionSignature
  }

  final case class FunctionFound(typeSig: FunctionsProviderSig, funSig: FunctionSignature) extends MethodResolutionResult {
    override def getFunSigOrThrow(): FunctionSignature = funSig
  }

  object ModuleNotFound extends MethodResolutionResult {
    override def getFunSigOrThrow(): FunctionSignature = throw new NoSuchElementException()
  }

  final case class FunctionNotFound(typeSig: FunctionsProviderSig) extends MethodResolutionResult {
    override def getFunSigOrThrow(): FunctionSignature = throw new NoSuchElementException()
  }

}
