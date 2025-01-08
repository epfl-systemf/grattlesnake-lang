package lang

import identifiers.{FunOrVarId, NormalFunOrVarId}
import lang.LanguageMode.OcapEnabled
import lang.Types.PrimitiveTypeShape.{BoolType, IntType, StringType, VoidType}

object FileSystemApi extends Device.DeviceApi {

  val openR: FunOrVarId = NormalFunOrVarId("openR")
  val openW: FunOrVarId = NormalFunOrVarId("openW")
  val openA: FunOrVarId = NormalFunOrVarId("openA")
  val write: FunOrVarId = NormalFunOrVarId("write")
  val read: FunOrVarId = NormalFunOrVarId("read")
  val close: FunOrVarId = NormalFunOrVarId("close")
  val createDir: FunOrVarId = NormalFunOrVarId("createDir")
  val delete: FunOrVarId = NormalFunOrVarId("delete")


  override def functions: Map[FunOrVarId, FunctionSignature] = Map(
    FunctionSignature(openR, List(None -> StringType), IntType, OcapEnabled).keyed,
    FunctionSignature(openW, List(None -> StringType), IntType, OcapEnabled).keyed,
    FunctionSignature(openA, List(None -> StringType), IntType, OcapEnabled).keyed,
    FunctionSignature(write, List(None -> IntType, None -> StringType), VoidType, OcapEnabled).keyed,
    FunctionSignature(read, List(None -> IntType), IntType, OcapEnabled).keyed,
    FunctionSignature(close, List(None -> IntType), VoidType, OcapEnabled).keyed,
    FunctionSignature(createDir, List(None -> StringType), BoolType, OcapEnabled).keyed,
    FunctionSignature(delete, List(None -> StringType), BoolType, OcapEnabled).keyed
  )

  extension (sig: FunctionSignature) private def keyed: (FunOrVarId, FunctionSignature) = {
    sig.name -> sig
  }
  
}
