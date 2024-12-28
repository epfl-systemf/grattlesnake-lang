package compiler.importscanner

import compiler.irs.Asts.{ModuleDef, Source}
import compiler.pipeline.CompilerStep
import identifiers.TypeIdentifier

final class ImportsScanner extends CompilerStep[List[Source], (List[Source], Map[TypeIdentifier, ModuleImports])] {

  override def apply(input: List[Source]): (List[Source], Map[TypeIdentifier, ModuleImports]) = {
    val importsB = Map.newBuilder[TypeIdentifier, ModuleImports]
    for {
      src <- input
      df <- src.defs
    } do {
      df match {
        case moduleDef: ModuleDef =>
          importsB.addOne(moduleDef.moduleName, ModuleImports.fromModuleDef(moduleDef))
        case _ => ()
      }
    }
    (input, importsB.result())
  }
  
}
