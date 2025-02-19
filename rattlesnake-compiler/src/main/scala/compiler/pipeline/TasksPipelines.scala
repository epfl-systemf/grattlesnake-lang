package compiler.pipeline

import compiler.analysisctx.{AnalysisContext, ContextCreator}
import compiler.backend.Backend
import compiler.controlflowchecker.ControlFlowChecker
import compiler.importscanner.ImportsScanner
import compiler.io.{SourceCodeProvider, StringWriter}
import compiler.irs.Asts
import compiler.lexer.Lexer
import compiler.lowerer.Lowerer
import compiler.parser.Parser
import compiler.prettyprinter.PrettyPrinter
import compiler.reporting.Errors.{ErrorReporter, ExitCode}
import compiler.tailrecchecker.TailrecChecker
import compiler.typechecker.TypeChecker
import identifiers.TypeIdentifier
import org.objectweb.asm.ClassVisitor

import java.nio.file.Path

/**
 * Contains methods producing pipelines for different tasks, indicated by their name
 */
object TasksPipelines {

  /**
   * Pipeline for compilation (src file -> .class file)
   */
  def compiler(
                outputDirectoryPath: Path,
                javaVersionCode: Int,
                runtimeDirPath: Path,
                agentDirPath: Path,
                er: ErrorReporter = defaultErrorReporter
              ): CompilerStep[List[SourceCodeProvider], List[TypeIdentifier]] = {
    compilerImpl(outputDirectoryPath, Backend.BinaryMode, javaVersionCode, runtimeDirPath, agentDirPath, er)
  }

  /**
   * Pipeline for bytecode writing (src file -> asm text file)
   */
  def bytecodeWriter(
                      outputDirectoryPath: Path,
                      javaVersionCode: Int,
                      runtimeDirPath: Path,
                      agentDirPath: Path,
                      er: ErrorReporter = defaultErrorReporter
                    ): CompilerStep[List[SourceCodeProvider], List[TypeIdentifier]] = {
    compilerImpl(outputDirectoryPath, Backend.AssemblyMode, javaVersionCode, runtimeDirPath, agentDirPath, er)
  }

  /**
   * Pipeline for formatting (src file -> formatted text file)
   */
  def formatter( // TODO should be able to handle comments
                 directoryPath: Path,
                 filename: String,
                 indentGranularity: Int,
                 overwriteFileCallback: String => Boolean,
                 displayAllParentheses: Boolean = false,
                 er: ErrorReporter = defaultErrorReporter
               ): CompilerStep[SourceCodeProvider, Unit] = {
    frontend(er)
      .andThen(new PrettyPrinter(indentGranularity, displayAllParentheses))
      .andThen(new StringWriter(directoryPath, filename, er, overwriteFileCallback))
  }

  /**
   * Pipeline for typechecker (src file -> side effects of error reporting)
   */
  def typeChecker(er: ErrorReporter = defaultErrorReporter,
                  okReporter: String => Unit = println): CompilerStep[List[SourceCodeProvider], Unit] = {
    MultiStep(frontend(er))
      .andThen(frontAnalyzer(er))
      .andThen(new Lowerer())
      .andThen(new TailrecChecker(er))
      .andThen(Mapper(_ => okReporter("no error found")))
  }

  // TODO should be able to support multi-files programs
  /**
   * Pipeline for lowering (src file -> lowered src file)
   */
  def lowerer(
               outputDirectoryPath: Path,
               filename: String,
               indentGranularity: Int = 2,
               overwriteFileCallback: String => Boolean,
               displayAllParentheses: Boolean = false,
               er: ErrorReporter = defaultErrorReporter
             ): CompilerStep[SourceCodeProvider, Unit] = {
    frontend(er)
      .andThen(Mapper(List(_)))
      .andThen(frontAnalyzer(er))
      .andThen(new Lowerer())
      .andThen(new TailrecChecker(er))
      .andThen(Mapper(_._1.head))
      .andThen(new PrettyPrinter(indentGranularity, displayAllParentheses))
      .andThen(new StringWriter(outputDirectoryPath, filename, er, overwriteFileCallback))
  }

  private def compilerImpl[V <: ClassVisitor](outputDirectoryPath: Path,
                                              backendMode: Backend.Mode[V],
                                              javaVersionCode: Int,
                                              runtimeDirPath: Path,
                                              agentDirPath: Path,
                                              er: ErrorReporter) = {
    MultiStep(frontend(er))
      .andThen(frontAnalyzer(er))
      .andThen(new Lowerer())
      .andThen(new TailrecChecker(er))
      .andThen(new Backend(
        backendMode,
        er,
        outputDirectoryPath,
        javaVersionCode,
        runtimeDirPath,
        agentDirPath
      ))
  }

  def frontend(er: ErrorReporter): CompilerStep[SourceCodeProvider, Asts.Source] = {
    new Lexer(er).andThen(new Parser(er))
  }

  private def frontAnalyzer(er: ErrorReporter): CompilerStep[List[Asts.Source], (List[Asts.Source], AnalysisContext)] = {
    new ImportsScanner()
      .andThen(new ContextCreator(er))
      .andThen(new TypeChecker(er))
      .andThen(new ControlFlowChecker(er))
  }

  private def defaultErrorReporter: ErrorReporter =
    new ErrorReporter(errorsConsumer = System.err.print, exit = defaultExit)

  private def defaultExit(exitCode: ExitCode): Nothing = {
    System.exit(exitCode)
    throw new AssertionError("cannot happen")
  }

}
