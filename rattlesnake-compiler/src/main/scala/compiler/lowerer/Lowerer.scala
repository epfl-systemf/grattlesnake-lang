package compiler.lowerer

import compiler.analysisctx.AnalysisContext
import compiler.irs.Asts.*
import compiler.pipeline.CompilerStep
import compiler.reporting.Position
import lang.Operator.*
import lang.Types.*
import lang.Types.PrimitiveTypeShape.*
import lang.{Intrinsics, Types}

/**
 * Lowering replaces (this list may not be complete):
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

  private def lower(src: Source): Source = propagatePosition(src.getPosition) {
    Source(src.defs.map(lower), src.languageMode).setName(src.getName)
  }

  private def lower(block: Block): Block = propagatePosition(block.getPosition) {
    Block(block.stats.map(lower))
  }

  private def lower(funDef: FunDef): FunDef = propagatePosition(funDef.getPosition) {
    val loweredFunDef = FunDef(funDef.funName, funDef.params.map(lower), funDef.optRetType, lower(funDef.body), funDef.isMain)
    loweredFunDef.setSignatureOpt(funDef.getSignatureOpt)
    loweredFunDef
  }

  private def lower(structDef: StructDef): StructDef = propagatePosition(structDef.getPosition) {
    StructDef(
      structDef.structName,
      structDef.fields.map(lower),
      structDef.directSupertypes,
      structDef.isInterface
    )
  }
  
  private def lower(moduleDef: ModuleDef): ModuleDef = propagatePosition(moduleDef.getPosition) {
    ModuleDef(
      moduleDef.moduleName,
      moduleDef.imports.map(lower),
      moduleDef.functions.map(lower)
    )
  }
  
  private def lower(packageDef: PackageDef): PackageDef = propagatePosition(packageDef.getPosition) {
    PackageDef(
      packageDef.packageName,
      packageDef.functions.map(lower)
    )
  }

  private def lower(constDef: ConstDef): ConstDef = ConstDef(
    constDef.constName,
    constDef.tpeOpt.map(lower),
    constDef.value  // no need to lower literals
  )

  private def lower(param: Param): Param = Param(
    param.paramNameOpt,
    lower(param.tpe),
    param.isReassignable
  )

  private def lower(imp: Import): Import = imp match {
    case ParamImport(paramId, paramType) =>
      ParamImport(paramId, lower(paramType))
    case packageImport: PackageImport => packageImport
    case deviceImport: DeviceImport => deviceImport
  }

  private def lower(localDef: LocalDef): LocalDef = propagatePosition(localDef.getPosition) {
    val loweredLocal = LocalDef(localDef.localName, localDef.optTypeAnnot.map(lower), localDef.rhsOpt.map(lower),
      localDef.isReassignable)
    loweredLocal.setVarTypeOpt(localDef.getVarTypeOpt)
    loweredLocal
  }

  private def lower(varAssig: VarAssig): VarAssig = propagatePosition(varAssig.getPosition) {
    VarAssig(lower(varAssig.lhs), lower(varAssig.rhs))
  }

  private def lower(varModif: VarModif): VarAssig = propagatePosition(varModif.getPosition) {
    val VarModif(lhs, rhs, op) = varModif
    val loweredLhs = lower(lhs)
    val loweredRhs = lower(rhs)
    VarAssig(loweredLhs, BinaryOp(loweredLhs, op, loweredRhs).setType(lhs.getType))
  }

  private def lower(ifThenElse: IfThenElse): IfThenElse = propagatePosition(ifThenElse.getPosition) {
    val lowered = IfThenElse(
      lower(ifThenElse.cond),
      lower(ifThenElse.thenBr),
      ifThenElse.elseBrOpt.map(lower).orElse {
        if ifThenElse.elseIsUnfeasible
        then Some(PanicStat(StringLit("incomplete pattern match (should never happen, compiler error)")))
        else None
      }
    )
    lowered.setSmartCasts(ifThenElse.getSmartCasts)
    lowered
  }

  private def lower(whileLoop: WhileLoop): WhileLoop = propagatePosition(whileLoop.getPosition) {
    WhileLoop(lower(whileLoop.cond), lower(whileLoop.body))
  }

  private def lower(forLoop: ForLoop): Block = propagatePosition(forLoop.getPosition) {
    val body = Block(
      forLoop.body.stats ++ forLoop.stepStats
    )
    val stats: List[Statement] = forLoop.initStats :+ WhileLoop(forLoop.cond, body)
    lower(Block(stats))
  }

  private def lower(returnStat: ReturnStat): ReturnStat = propagatePosition(returnStat.getPosition) {
    ReturnStat(returnStat.optVal.map(lower))
  }

  private def lower(panicStat: PanicStat): PanicStat = propagatePosition(panicStat.getPosition) {
    PanicStat(lower(panicStat.msg))
  }

  private def lower(expr: Expr): Expr = propagatePosition(expr.getPosition) {
    val lowered = expr match {
      case literal: Literal => literal
      case varRef: VariableRef => varRef
      case meRef: MeRef => meRef
      case packageRef: PackageRef => packageRef
      case deviceRef: DeviceRef => deviceRef
      case call: Call => lower(call)
      case indexing: Indexing => Indexing(lower(indexing.indexed), lower(indexing.arg))
      case arrayInit: ArrayInit => ArrayInit(arrayInit.regionOpt.map(lower), arrayInit.elemType, lower(arrayInit.size))
      case instantiation: StructOrModuleInstantiation =>
        StructOrModuleInstantiation(instantiation.regionOpt.map(lower), instantiation.typeId, instantiation.args.map(lower))
      case regionCreation: RegionCreation => regionCreation
      
      // [x_1, ... , x_n] ---> explicit assignments
      case filledArrayInit@FilledArrayInit(arrayElems, regionOpt) =>
        /* Argument for soundness of creating a region here:
         * This creation happens only when the region is not specified in the original AST, i.e. when the array is 
         * meant to be immutable. The type of the expression is therefore an immutable array type that captures no 
         * region, hence the region does not exit the code in the Sequence. */
        val region = regionOpt.getOrElse(RegionCreation())
        val arrayType = filledArrayInit.getType
        val arrayShape = arrayType.shape.asInstanceOf[ArrayTypeShape]
        val elemType = arrayShape.elemType
        val arrValId = uniqueIdGenerator.next()
        val arrValRef = VariableRef(arrValId).setType(arrayType)
        val arrInit = ArrayInit(Some(region), WrapperTypeTree(elemType), IntLit(arrayElems.size)).setType(arrayType)
        val arrayValDefinition = LocalDef(arrValId, Some(WrapperTypeTree(arrayType)), Some(arrInit), isReassignable = false)
        arrayValDefinition.setVarType(arrayType)
        val arrElemAssigStats = arrayElems.map(lower).zipWithIndex.map {
          (elem, idx) => VarAssig(Indexing(arrValRef, IntLit(idx)).setType(UndefinedTypeShape), elem)
        }
        Sequence(arrayValDefinition :: arrElemAssigStats, arrValRef)
        
      case UnaryOp(operator, operand) =>
        val loweredOperand = lower(operand)
        operator match {
          case ExclamationMark => Ternary(loweredOperand, BoolLit(false), BoolLit(true))
          case _ => UnaryOp(operator, loweredOperand)
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
          case And =>
            val ternary = Ternary(loweredLhs, loweredRhs, BoolLit(false))
            ternary.setSmartCasts(binaryOp.getSmartCasts)
            lower(ternary)
          
          // x || y ---> when x then true else y
          case Or => lower(Ternary(loweredLhs, BoolLit(true), loweredRhs))
          
          // nothing to lower at top-level, only perform recursive calls
          case _ => BinaryOp(loweredLhs, binaryOp.operator, loweredRhs)
        }
      }
      case select: Select => Select(lower(select.lhs), select.selected)

      // need to treat separately the case where one of the branches does not return (o.w. Java ASM crashes)
      case ternary@Ternary(cond, thenBr, elseBr) if thenBr.getType == NothingType || elseBr.getType == NothingType => {
        val valName = uniqueIdGenerator.next()
        if (thenBr.getType == NothingType){
          val ifStat = IfThenElse(cond, thenBr, None)
          ifStat.setSmartCasts(ternary.getSmartCasts)
          lower(Sequence(List(ifStat), elseBr))
        } else {
          val ifStat = IfThenElse(UnaryOp(ExclamationMark, cond).setType(BoolType), elseBr, None)
          lower(Sequence(List(ifStat), thenBr))
        }
      }
      case ternary@Ternary(cond, thenBr, elseBr) =>
        val loweredTernary = Ternary(lower(cond), lower(thenBr), lower(elseBr))
        loweredTernary.setSmartCasts(ternary.getSmartCasts)
        loweredTernary
      case Cast(expr, tpe) => Cast(lower(expr), tpe)
      case TypeTest(expr, tpe) => TypeTest(lower(expr), tpe)
      case Sequence(stats, expr) => Sequence(stats.map(lower), lower(expr))
    }
    lowered.setTypeOpt(expr.getTypeOpt)
  }

  private def lower(call: Call): Call = propagatePosition(call.getPosition){
    val loweredReceiverOpt = call.receiverOpt.map(lower).orElse {
      if Intrinsics.intrinsics.contains(call.getSignatureOpt.get.name)
      then None
      else Some(MeRef().setTypeOpt(call.getMeTypeOpt))
    }
    val loweredCall = Call(loweredReceiverOpt, call.function, call.args.map(lower), call.isTailrec)
    loweredCall.setResolvedSigOpt(call.getSignatureOpt)
    loweredCall.setTypeOpt(call.getTypeOpt)
    loweredCall
  }

  private def lower(statement: Statement): Statement = propagatePosition(statement.getPosition) {
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
      case restrictedStat: RestrictedStat => lower(restrictedStat)
      case enclosedStat: EnclosedStat => lower(enclosedStat)
  }

  private def lower(topLevelDef: TopLevelDef): TopLevelDef = propagatePosition(topLevelDef.getPosition) {
    topLevelDef match
      case moduleDef: ModuleDef => lower(moduleDef)
      case packageDef: PackageDef => lower(packageDef)
      case structDef: StructDef => lower(structDef)
      case constDef: ConstDef => lower(constDef)
  }
  
  private def lower(restrictedStat: RestrictedStat): RestrictedStat = propagatePosition(restrictedStat.getPosition) {
    RestrictedStat(lower(restrictedStat.captureSet), lower(restrictedStat.body))
  }
  
  private def lower(enclosedStat: EnclosedStat): EnclosedStat = propagatePosition(enclosedStat.getPosition) {
    EnclosedStat(lower(enclosedStat.captureSet), lower(enclosedStat.body))
  }
  
  private def lower(tpe: TypeTree): TypeTree = tpe match {
    case CapturingTypeTree(typeShapeTree, captureDescr) =>
      CapturingTypeTree(lower(typeShapeTree), lower(captureDescr))
    case shape: TypeShapeTree => lower(shape)
    case wrapperTypeTree: WrapperTypeTree => wrapperTypeTree
  }
  
  private def lower(shape: TypeShapeTree): TypeShapeTree = shape match {
    case shape: CastTargetTypeShapeTree => shape
    case ArrayTypeShapeTree(elemType, isModifiable) =>
      ArrayTypeShapeTree(lower(elemType), isModifiable)
  }
  
  private def lower(captureSetTree: ExplicitCaptureSetTree): ExplicitCaptureSetTree = propagatePosition(captureSetTree.getPosition) {
    val lowered = ExplicitCaptureSetTree(captureSetTree.capturedExpressions.map(lower))
    lowered.setResolvedDescrOpt(captureSetTree.getResolvedDescrOpt)
    lowered
  }

  private def lower(captureDescrTree: CaptureDescrTree): CaptureDescrTree = propagatePosition(captureDescrTree.getPosition) {
    val loweredCapDescr = captureDescrTree match {
      case ExplicitCaptureSetTree(capturedExpressions) =>
        ExplicitCaptureSetTree(capturedExpressions.map(lower))
      case implicitRootCaptureSetTree: ImplicitRootCaptureSetTree => implicitRootCaptureSetTree
      case brandTree: MarkTree => brandTree
    }
    loweredCapDescr.setResolvedDescrOpt(captureDescrTree.getResolvedDescrOpt)
    loweredCapDescr
  }

  private def propagatePosition[A <: Ast](pos: Option[Position], maxDepth: Int = 2)(ast: A): A = {
    if (maxDepth > 0 && ast.getPosition.isEmpty){
      ast.setPosition(pos)
      for (child <- ast.children){
        propagatePosition(pos, maxDepth - 1)(child)
      }
    }
    ast
  }

}
