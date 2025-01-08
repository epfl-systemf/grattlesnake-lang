package lang

import identifiers.{FunOrVarId, NormalFunOrVarId}
import lang.LanguageMode.OcapEnabled
import lang.Types.PrimitiveTypeShape.{StringType, VoidType}

object ConsoleApi extends Device.DeviceApi {

  val print: FunOrVarId = NormalFunOrVarId("print")
  val readLine: FunOrVarId = NormalFunOrVarId("readLine")

  override def functions: Map[FunOrVarId, FunctionSignature] = Map(
    FunctionSignature(print, List(None -> StringType), VoidType, OcapEnabled).keyed,
    FunctionSignature(readLine, List.empty, StringType, OcapEnabled).keyed
  )

  extension (sig: FunctionSignature) private def keyed: (FunOrVarId, FunctionSignature) = {
    sig.name -> sig
  }
  
}
