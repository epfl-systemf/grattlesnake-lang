package compiler.lowerer

import compiler.irs.Asts.*
import compiler.{AnalysisContext, CompilerStep, FunctionsToInject}
import lang.Operator.*
import lang.Operators
import lang.Types.PrimitiveType.*
import lang.Types.{ArrayType, UndefinedType}
import identifiers.StringEqualityFunId

/**
 * Lowering replaces:
 *  - `>`, `>=` ---> reversed
 *  - `x <= y` ---> `(x < y) || (x == y)`
 *  - `x != y` ---> `!(x == y)`
 *  - `VarModif`: `x += y` ---> `x = x + y`
 *  - `for` ---> `while`
 *  - `-x` ---> `0 - x`
 *  - `!x` ---> `when x then false else true`
 *  - `x && y` ---> `when x then y else false`
 *  - `x || y` ---> `when x then true else y`
 *  - `[x_1, ... , x_n]` ---> `val $0 = arr Int[n]; $0[0] = x_1; ... ; $0[n-1] = x_n; $0`
 */
final class Lowerer extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {
  private val uniqueIdGenerator = new UniqueIdGenerator()
  
  /*
  * =========================================================================
  * IMPORTANT: recursive calls to lower(...) must be performed everywhere,
  * including on ASTs generated by another call to lower (some expressions, 
  * e.g. `x >= y`, take several steps to be completely lowered)
  * =========================================================================
  */

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, ctx) = input
    val loweredSources = sources.map(lower)
    loweredSources.foreach(_.assertAllTypesAreSet())
    (loweredSources, ctx)
  }

  private def lower(src: Source): Source = Source(src.defs.map(lower)).setName(src.getName)

  private def lower(block: Block): Block = Block(block.stats.map(lower))

  private def lower(funDef: FunDef): FunDef = {
    FunDef(funDef.funName, funDef.params.map(lower), funDef.optRetType, lower(funDef.body))
  }

  private def lower(structDef: StructDef): StructDef = {
    StructDef(structDef.structName, structDef.fields.map(lower))
  }
  
  private def lower(testDef: TestDef): TestDef = {
    TestDef(testDef.testName, lower(testDef.body))
  }

  private def lower(constDef: ConstDef): ConstDef = constDef

  private def lower(param: Param): Param = param

  private def lower(localDef: LocalDef): LocalDef =
    LocalDef(localDef.localName, localDef.optType, lower(localDef.rhs), localDef.isReassignable)

  private def lower(varAssig: VarAssig): VarAssig = VarAssig(lower(varAssig.lhs), lower(varAssig.rhs))

  private def lower(varModif: VarModif): VarAssig = {
    val VarModif(lhs, rhs, op) = varModif
    val loweredLhs = lower(lhs)
    val loweredRhs = lower(rhs)
    VarAssig(loweredLhs, BinaryOp(loweredLhs, op, loweredRhs).setType(lhs.getType))
  }

  private def lower(ifThenElse: IfThenElse): IfThenElse = {
    IfThenElse(lower(ifThenElse.cond), lower(ifThenElse.thenBr), ifThenElse.elseBrOpt.map(lower))
  }

  private def lower(whileLoop: WhileLoop): WhileLoop = {
    WhileLoop(lower(whileLoop.cond), lower(whileLoop.body))
  }

  private def lower(forLoop: ForLoop): Block = {
    val body = Block(
      forLoop.body.stats ++ forLoop.stepStats
    )
    val stats: List[Statement] = forLoop.initStats :+ WhileLoop(forLoop.cond, body)
    lower(Block(stats))
  }

  private def lower(returnStat: ReturnStat): ReturnStat = ReturnStat(returnStat.optVal.map(lower))

  private def lower(panicStat: PanicStat): PanicStat = PanicStat(lower(panicStat.msg))

  private def lower(expr: Expr): Expr = {
    val lowered = expr match {
      case literal: Literal => literal
      case varRef: VariableRef => varRef
      case call: Call => Call(lower(call.callee), call.args.map(lower))
      case indexing: Indexing => Indexing(lower(indexing.indexed), lower(indexing.arg))
      case arrayInit: ArrayInit => ArrayInit(arrayInit.elemType, lower(arrayInit.size))
      case structInit: StructInit => StructInit(structInit.structName, structInit.args.map(lower), structInit.modifiable)
      
      // [x_1, ... , x_n] ---> explicit assignments
      case filledArrayInit@FilledArrayInit(arrayElems, _) =>
        val arrayType = filledArrayInit.getType.asInstanceOf[ArrayType]
        val elemType = arrayType.elemType
        val arrValId = uniqueIdGenerator.next()
        val arrValRef = VariableRef(arrValId).setType(arrayType)
        val arrInit = ArrayInit(elemType, IntLit(arrayElems.size)).setType(filledArrayInit.getType)
        val arrayValDefinition = LocalDef(arrValId, Some(arrayType), arrInit, isReassignable = false)
        val arrElemAssigStats = arrayElems.map(lower).zipWithIndex.map {
          (elem, idx) => VarAssig(Indexing(arrValRef, IntLit(idx)).setType(UndefinedType), elem)
        }
        Sequence(arrayValDefinition :: arrElemAssigStats, arrValRef)
        
      case UnaryOp(operator, operand) =>
        val loweredOperand = lower(operand)
        operator match {
          case ExclamationMark => Ternary(loweredOperand, BoolLit(false), BoolLit(true))
          case _ => UnaryOp(operator, loweredOperand)
        }

      case BinaryOp(lhs, Equality, rhs) if lhs.getType == StringType => {
        val loweredLhs = lower(lhs)
        val loweredRhs = lower(rhs)
        Call(
          VariableRef(StringEqualityFunId).setType(UndefinedType),
          List(loweredLhs, loweredRhs)
        ).setType(BoolType)
      }
        
      case binaryOp: BinaryOp => {
        val loweredLhs = lower(binaryOp.lhs)
        val loweredRhs = lower(binaryOp.rhs)
        binaryOp.operator match {
          
          // x <= y ---> x <= y || x == y
          case LessOrEq => lower(BinaryOp(
            BinaryOp(loweredLhs, LessThan, loweredRhs).setType(BoolType),
            Or,
            BinaryOp(loweredLhs, Equality, loweredRhs).setType(BoolType)
          ))
          
          // x > y ---> y < x  (and similar with >=)
          case GreaterThan => lower(BinaryOp(loweredRhs, LessThan, loweredLhs))
          case GreaterOrEq => lower(BinaryOp(loweredRhs, LessOrEq, loweredLhs))
          
          // x != y ---> !(x == y)
          case Inequality =>
            lower(UnaryOp(ExclamationMark,
              BinaryOp(loweredLhs, Equality, loweredRhs).setType(BoolType)
            ).setType(BoolType))
            
          // x && y ---> when x then y else false
          case And => lower(Ternary(loweredLhs, loweredRhs, BoolLit(false)))
          
          // x || y ---> when x then true else y
          case Or => lower(Ternary(loweredLhs, BoolLit(true), loweredRhs))
          
          // nothing to lower at top-level, only perform recursive calls
          case _ => BinaryOp(loweredLhs, binaryOp.operator, loweredRhs)
        }
      }
      case select: Select => Select(lower(select.lhs), select.selected)

      // need to treat separately the case where one of the branches does not return (o.w. Java ASM crashes)
      case Ternary(cond, thenBr, elseBr) if thenBr.getType == NothingType || elseBr.getType == NothingType => {
        val valName = uniqueIdGenerator.next()
        if (thenBr.getType == NothingType){
          val ifStat = IfThenElse(cond, thenBr, None)
          lower(Sequence(List(ifStat), elseBr))
        } else {
          val ifStat = IfThenElse(UnaryOp(ExclamationMark, cond).setType(BoolType), elseBr, None)
          lower(Sequence(List(ifStat), thenBr))
        }
      }
      case Ternary(cond, thenBr, elseBr) => Ternary(lower(cond), lower(thenBr), lower(elseBr))
      case Cast(expr, tpe) => Cast(lower(expr), tpe)
      case Sequence(stats, expr) => Sequence(stats.map(lower), lower(expr))
    }
    lowered.setTypeOpt(expr.getTypeOpt)
  }

  private def lower(statement: Statement): Statement = {
    // call appropriate method for each type of statement
    statement match
      case expr: Expr => lower(expr)
      case block: Block => lower(block)
      case localDef: LocalDef => lower(localDef)
      case varAssig: VarAssig => lower(varAssig)
      case varModif: VarModif => lower(varModif)
      case ifThenElse: IfThenElse => lower(ifThenElse)
      case whileLoop: WhileLoop => lower(whileLoop)
      case forLoop: ForLoop => lower(forLoop)
      case returnStat: ReturnStat => lower(returnStat)
      case panicStat: PanicStat => lower(panicStat)
  }

  private def lower(topLevelDef: TopLevelDef): TopLevelDef = {
    topLevelDef match
      case funDef: FunDef => lower(funDef)
      case structDef: StructDef => lower(structDef)
      case testDef: TestDef => lower(testDef)
      case constDef: ConstDef => lower(constDef)
  }

}
