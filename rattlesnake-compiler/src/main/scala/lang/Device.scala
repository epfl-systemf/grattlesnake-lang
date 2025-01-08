package lang

import identifiers.{FunOrVarId, NormalTypeId, TypeIdentifier}
import lang.CaptureDescriptors.CaptureSet
import lang.Types.{NamedTypeShape, Type}

enum Device(val keyword: Keyword, val typeName: TypeIdentifier, val api: Device.DeviceApi) {
  
  case Console extends Device(Keyword.Console, NormalTypeId("Console"), ConsoleApi)
  case FileSystem extends Device(Keyword.Fs, NormalTypeId("FileSystem"), FileSystemApi)
  
  def tpe: Type = NamedTypeShape(typeName) ^ CaptureSet.singletonOfRoot
  
  override def toString: String = keyword.str
}

object Device {
  
  trait DeviceApi {
    def functions: Map[FunOrVarId, FunctionSignature]
  }
  
  val kwToDevice: Map[Keyword, Device] = values.map(d => d.keyword -> d).toMap
  
  val devicesTypes: Set[TypeIdentifier] = values.map(_.typeName).toSet
  
}
