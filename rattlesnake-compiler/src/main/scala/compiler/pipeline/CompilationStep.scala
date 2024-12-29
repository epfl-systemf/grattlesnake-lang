package compiler.pipeline

enum CompilationStep {
  case Lexing
  case Parsing
  case ImportsScanning
  case ContextCreation
  case TypeChecking
  case PathsChecking
  case TailrecChecking
  case CodeGeneration
  case SourceFileWriting
}
