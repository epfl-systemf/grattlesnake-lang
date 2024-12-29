package compiler.tailrecchecker

import compiler.analysisctx.AnalysisContext
import compiler.irs.Asts
import compiler.irs.Asts.*
import compiler.pipeline.CompilationStep.TailrecChecking
import compiler.pipeline.CompilerStep
import compiler.reporting.Errors.{Err, ErrorReporter}

final class TailrecChecker(errorReporter: ErrorReporter)
  extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val sources = input._1
    for (src <- sources){
      check(src, false)
    }
    errorReporter.displayAndTerminateIfErrors()
    input
  }

  private def check(ast: Ast, isTailPosition: Boolean): Unit = {
    ast match {
      case Call(receiverOpt, funName, args, isTailrec) if isTailrec && !isTailPosition =>
        errorReporter.push(Err(TailrecChecking, s"call to '$funName' is not in tail position", ast.getPosition))
        args.foreach(check(_, false))
      case Ternary(cond, thenBr, elseBr) =>
        check(cond, false)
        check(thenBr, isTailPosition)
        check(elseBr, isTailPosition)
      case ReturnStat(Some(expr)) =>
        check(expr, true)
      case _ =>
        ast.children.foreach(check(_, false))
    }
  }

}