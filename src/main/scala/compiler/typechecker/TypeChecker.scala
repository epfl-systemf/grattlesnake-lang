package compiler.typechecker

import compiler.analysisctx.AnalysisContext
import compiler.analysisctx.AnalysisContext.{FunctionFound, FunctionNotFound, ModuleNotFound}
import compiler.gennames.NamesForGeneratedClasses.constantsClassName
import compiler.irs.Asts.*
import compiler.pipeline.CompilationStep.TypeChecking
import compiler.pipeline.CompilerStep
import compiler.reporting.Errors.{Err, ErrorReporter, Warning}
import compiler.reporting.Position
import compiler.typechecker.SubcaptureRelation.{isCoveredBy, subcaptureOf}
import compiler.typechecker.SubtypeRelation.subtypeOf
import compiler.typechecker.TypeCheckingContext.LocalInfo
import identifiers.{FunOrVarId, IntrinsicsPackageId, NormalTypeId, TypeIdentifier}
import lang.*
import lang.Capturables.*
import lang.CaptureDescriptors.{Brand, CaptureDescriptor, CaptureSet}
import lang.Operator.{Equality, Inequality, Sharp}
import lang.Operators.{BinaryOpSignature, UnaryOpSignature, binaryOperators, unaryOperators}
import lang.StructSignature.FieldInfo
import lang.Types.*
import lang.Types.PrimitiveTypeShape.*

import scala.collection.mutable

final class TypeChecker(errorReporter: ErrorReporter)
  extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, analysisContext) = input
    for src <- sources do {
      checkSource(src, analysisContext)
    }
    errorReporter.displayAndTerminateIfErrors()
    sources.foreach(_.assertAllTypesAreSet())
    input
  }

  private def checkSource(src: Source, analysisContext: AnalysisContext): Unit = {
    val Source(defs) = src
    for (df <- defs) {
      checkTopLevelDef(df, analysisContext)
    }
  }

  private def checkTopLevelDef(topLevelDef: TopLevelDef, analysisContext: AnalysisContext): Unit = topLevelDef match {

    case PackageDef(packageName, functions) =>
      val packageSig = analysisContext.resolveType(packageName).get.asInstanceOf[PackageSignature]
      val environment = Environment(
        packageSig.importedPackages.toSet,
        packageSig.importedDevices.toSet
      )
      for func <- functions do {
        checkFunction(func, analysisContext, packageName, packageSig.getCaptureDescr, environment)
      }

    case ModuleDef(moduleName, imports, functions) =>
      val moduleSig = analysisContext.resolveType(moduleName).get.asInstanceOf[ModuleSignature]
      val tcCtx = TypeCheckingContext(analysisContext, moduleName, moduleSig.getCaptureDescr)
      for imp <- imports do {
        checkImport(imp, tcCtx)
      }
      val environment = Environment(
        moduleSig.importedPackages.toSet,
        moduleSig.importedDevices.toSet
      )
      for func <- functions do {
        checkFunction(func, analysisContext, moduleName, moduleSig.getCaptureDescr, environment)
      }

    case StructDef(structName, fields, _, _) =>
      val structSig = analysisContext.resolveType(structName).get.asInstanceOf[StructSignature]
      val tcCtx = TypeCheckingContext(analysisContext, structName, structSig.getCaptureDescr)
      for (param@Param(paramNameOpt, typeTree, isReassignable) <- fields) {
        val tpe = checkType(typeTree)(using tcCtx, analysisContext.unrestrictedEnvironment)
        paramNameOpt.foreach { paramName =>
          tcCtx.addLocal(paramName, tpe, param.getPosition, isReassignable, declHasTypeAnnot = true,
            duplicateVarCallback = { () =>
              reportError(s"duplicate field: $paramName", param.getPosition)
            },
            forbiddenTypeCallback = { () =>
              reportError(s"field $paramName has type $tpe, which is forbidden", param.getPosition)
            }
          )
        }
      }

    case constDef@ConstDef(constName, tpeOpt, value) =>
      val inferredType = checkLiteralExpr(value)
      tpeOpt.foreach { expType =>
        val placeholderMeId = NormalTypeId(constantsClassName)
        val tcCtx = TypeCheckingContext(analysisContext, placeholderMeId, CaptureSet.empty)
        val checkedType = checkType(expType)(using tcCtx, Environment.empty)
        checkSubtypingConstraint(
          checkedType,
          inferredType,
          constDef.getPosition,
          "constant definition",
          tcCtx
        )(using tcCtx, Environment.empty)
      }
  }

  private def checkFunction(
                             funDef: FunDef,
                             analysisContext: AnalysisContext,
                             meId: TypeIdentifier,
                             meCaptureDescr: CaptureDescriptor,
                             environment: Environment
                           ): Unit = {
    val FunDef(funName, params, optRetTypeTree, body) = funDef
    val tcCtx = TypeCheckingContext(analysisContext, meId, meCaptureDescr)
    for param <- params do {
      val paramType = checkType(param.tpe)(using tcCtx, environment)
      param.paramNameOpt.foreach { paramName =>
        tcCtx.addLocal(paramName, paramType, param.getPosition, param.isReassignable, declHasTypeAnnot = true,
          duplicateVarCallback = { () =>
            reportError(s"identifier '${param.paramNameOpt}' is already used by another parameter of function '$funName'", param.getPosition)
          }, forbiddenTypeCallback = { () =>
            reportError(s"parameter '${param.paramNameOpt}' of function '$funName' has type '$paramType', which is forbidden", param.getPosition)
          })
      }
    }
    val optRetType = optRetTypeTree.map(checkType(_)(using tcCtx, environment))
    val expRetType = optRetType.getOrElse(VoidType)
    checkStat(body)(using tcCtx, environment, expRetType)
    val endStatus = checkReturns(body)
    if (!endStatus.alwaysStopped && !expRetType.subtypeOf(VoidType)(using tcCtx)) {
      reportError("missing return in non-Void function", funDef.getPosition)
    }
    if (expRetType == NothingType && !endStatus.alwaysStopped) {
      reportError(s"cannot prove that function '$funName' with return type '$NothingType' cannot return", funDef.getPosition)
    }
    tcCtx.writeLocalsRelatedWarnings(errorReporter)
  }

  private def checkImport(imp: Import, tcCtx: TypeCheckingContext): Unit = imp match {
    case modImp@ParamImport(paramName, paramType) =>
      val tpe = checkType(paramType)(using tcCtx, tcCtx.unrestrictedEnvironment)
      tcCtx.addLocal(paramName, tpe, modImp.getPosition, isReassignable = false, declHasTypeAnnot = true,
        duplicateVarCallback = { () =>
          reportError(s"duplicated parameter: $paramName", modImp.getPosition)
        },
        forbiddenTypeCallback = { () =>
          reportError(s"module $paramName has type $paramType, which is forbidden", modImp.getPosition)
        }
      )
    case pkgImp@PackageImport(packageId) =>
      if (!tcCtx.knowsUserDefType(packageId)) {
        reportError(s"unknown package: $packageId", imp.getPosition)
      }
    case DeviceImport(device) => ()
  }

  private def checkType(typeTree: TypeTree)(using tcCtx: TypeCheckingContext, env: Environment): Type = {
    val tpe = typeTree match {
      case CapturingTypeTree(typeShapeTree, captureDescrTree) =>
        val shape = checkTypeShape(typeShapeTree)
        val descriptor = checkCaptureDescr(captureDescrTree)
        warnOnNonCapAndRedundantCaptures(captureDescrTree)
        CapturingType(shape, descriptor)
      case typeShapeTree: TypeShapeTree =>
        checkTypeShape(typeShapeTree)
      case WrapperTypeTree(tpe) => tpe
    }
    warnWhenPrimitiveHasInappropriateCaptureDescr(tpe, typeTree.getPosition)
    tpe
  }

  private def warnWhenPrimitiveHasInappropriateCaptureDescr(tpe: Type, posOpt: Option[Position])
                                                           (using tcCtx: TypeCheckingContext, env: Environment): Unit = {
    tpe.shape match {
      case RegionType if !CaptureDescriptors.singletonSetOfRoot.subcaptureOf(tpe.captureDescriptor) =>
        reportError(
          s"type $tpe is not inhabited, as a region always captures the root capability",
          posOpt,
          isWarning = true
        )
      case primType: PrimitiveTypeShape if primType != RegionType && !tpe.captureDescriptor.isEmpty =>
        reportError(
          s"capture set is useless, as values of type ${tpe.shape} never capture capabilities",
          posOpt,
          isWarning = true
        )
      case _ => ()
    }
  }

  private def warnOnNonCapAndRedundantCaptures(captureDescrTree: CaptureDescrTree)
                                     (using tcCtx: TypeCheckingContext, env: Environment): Unit = {
    captureDescrTree match {
      case ExplicitCaptureSetTree(capturedExpressions) =>
        var foundNotCap = false
        capturedExpressions.foreach { captExpr =>
          if (captExpr.getType.captureDescriptor.isEmpty) {
            reportError("captured expression is not a capability", captExpr.getPosition, isWarning = true)
            foundNotCap = true
          }
        }
        if (!foundNotCap){
          // do not try to find redundant capabilities if we have already found a problem on this capture set
          captureDescrTree.getResolvedDescrOpt.foreach {
            case CaptureSet(set) =>
              set.find(c => c.isCoveredBy(CaptureSet(set - c)))
                .foreach { capability =>
                  reportError(
                    s"redundant capability: $capability is already covered by the rest of the capture set",
                    captureDescrTree.getPosition,
                    isWarning = true
                  )
                }
            case _ => ()
          }
        }
      case _ => ()
    }
  }

  private def checkTypeShape(typeShapeTree: TypeShapeTree)
                            (using tcCtx: TypeCheckingContext, env: Environment): TypeShape = typeShapeTree match {
    case ArrayTypeShapeTree(elemType, isModifiable) =>
      ArrayTypeShape(checkType(elemType), isModifiable)
    case castTargetTypeShapeTree: CastTargetTypeShapeTree =>
      checkCastTargetTypeShape(castTargetTypeShapeTree)
  }

  private def checkCastTargetTypeShape(castTargetTypeShapeTree: CastTargetTypeShapeTree)
                                      (using tcCtx: TypeCheckingContext, env: Environment): CastTargetTypeShape = {
    castTargetTypeShapeTree match {
      case PrimitiveTypeShapeTree(primitiveType) =>
        primitiveType
      case NamedTypeShapeTree(name) =>
        if (!tcCtx.knowsUserDefType(name)) {
          reportError(s"unknown: $name", castTargetTypeShapeTree.getPosition)
        }
        NamedTypeShape(name)
    }
  }

  private def checkCaptureDescr(captureDescrTree: CaptureDescrTree)
                               (using tcCtx: TypeCheckingContext, env: Environment): CaptureDescriptor = {
    val captureDescr = captureDescrTree match {
      case ExplicitCaptureSetTree(capturedExpressions) =>
        CaptureSet(capturedExpressions.flatMap { expr =>
          checkCapturedExpr(expr, errorReporter)(using tcCtx)
        }.toSet)
      case ImplicitRootCaptureSetTree() => CaptureSet.singletonOfRoot
      case BrandTree() => Brand
    }
    captureDescrTree.setResolvedDescr(captureDescr)
    captureDescr
  }

  private def checkCapturedExpr(expr: Expr, er: ErrorReporter)
                               (using tcCtx: TypeCheckingContext, envir: Environment): Option[ConcreteCapturable] = {

    def reportError(msg: String, posOpt: Option[Position]): None.type = {
      er.push(Err(TypeChecking, msg, posOpt))
      None
    }

    checkExpr(expr)
    expr match {
      case VariableRef(name) =>
        Some(IdPath(name))
      case MeRef() => Some(MePath)
      case PackageRef(pkgName) => Some(CapPackage(pkgName))
      case DeviceRef(device) => Some(CapDevice(device))
      case Select(lhs, selected) =>
        checkCapturedExpr(lhs, er).flatMap {
          case lhsPath: Path => {
            lhs.getType match {
              case NamedTypeShape(lhsTypeId) =>
                tcCtx.resolveType(lhsTypeId).flatMap { lhsTypeSig =>
                  lhsTypeSig.params.get(selected).flatMap { fieldInfo =>
                    if fieldInfo.isReassignable
                    then reportError(s"cannot capture field $selected of $lhsTypeId, as it is reassignable", expr.getPosition)
                    else Some(lhsPath.dot(selected))
                  }
                }
              case lhsType => None
            }
          }
          case _ => None
        }
      case _ => reportError(s"not a path", expr.getPosition)
    }
  }

  private def checkStat(statement: Statement)
                       (using tcCtx: TypeCheckingContext, env: Environment, expRetType: Type): Unit = statement match {

    case expr: Expr => checkExpr(expr)

    case Block(stats) =>
      val newCtx = tcCtx.copied
      for stat <- stats do {
        checkStat(stat)(using newCtx)
      }
      newCtx.writeLocalsRelatedWarnings(errorReporter)

    case localDef@LocalDef(localName, optTypeAnnotTree, rhs, isReassignable) =>
      val inferredType = checkExpr(rhs)
      val optAnnotType = optTypeAnnotTree.map { typeAnnot =>
        val tpe = checkType(typeAnnot)(using tcCtx, tcCtx.unrestrictedEnvironment)
        checkSubtypingConstraint(tpe, inferredType, localDef.getPosition, "local definition", tcCtx)
        tpe
      }
      val requestExplicitType = optTypeAnnotTree.isEmpty && inferredType.isInstanceOf[UnionTypeShape]
      if (requestExplicitType) {
        reportError(s"Please provide an explicit type for $localName", localDef.getPosition)
      }
      val actualType = if requestExplicitType then UndefinedTypeShape else optAnnotType.getOrElse(inferredType)
      localDef.setVarType(actualType)
      tcCtx.addLocal(localName, actualType, localDef.getPosition, isReassignable,
        declHasTypeAnnot = optTypeAnnotTree.isDefined,
        duplicateVarCallback = { () =>
          reportError(s"'$localName' is already defined in this scope", localDef.getPosition)
        }, forbiddenTypeCallback = { () =>
          reportError(s"${localDef.keyword} '$localName' has type '$actualType', which is forbidden", localDef.getPosition)
        })

    case varAssig@VarAssig(lhs, rhs) =>
      val rhsType = checkExpr(rhs)
      lhs match {
        case varRef@VariableRef(name) =>
          tcCtx.varIsAssigned(name)
          val varType = mustBeReassignable(name, tcCtx, varAssig.getPosition)
          checkSubtypingConstraint(varType, rhsType, varAssig.getPosition, "", tcCtx)
          varRef.setType(varType)
        case indexing@Indexing(indexed, _) =>
          checkExpr(indexing)
          val lhsType = exprMustBeArray(indexed.getType, tcCtx, varAssig.getPosition, mustUpdate = true)
          checkSubtypingConstraint(lhsType, rhsType, varAssig.getPosition, "", tcCtx)
        case select@Select(structExpr, selected) =>
          val structType = checkExpr(structExpr)
          val fieldType = exprMustHaveField(structExpr, selected, tcCtx, varAssig.getPosition, mustUpdateField = true)
          checkSubtypingConstraint(fieldType, rhsType, varAssig.getPosition, "", tcCtx)
          select.setType(fieldType)
        case _ =>
          reportError("syntax error: only variables, struct fields and array elements can be assigned", varAssig.getPosition)
      }

    case varModif@VarModif(lhs, rhs, op) =>
      // no check for unused mut because no operator combinable with = can operate on mutable types
      val rhsType = checkExpr(rhs)
      lhs match {
        case varRef@VariableRef(name) =>
          tcCtx.varIsAssigned(name)
          val inPlaceModifiedType = mustBeReassignable(name, tcCtx, varModif.getPosition)
          val operatorRetType = mustExistOperator(inPlaceModifiedType, op, rhsType, varModif.getPosition)
          checkSubtypingConstraint(inPlaceModifiedType, operatorRetType, varModif.getPosition, "", tcCtx)
          varRef.setType(inPlaceModifiedType)
        case indexing@Indexing(indexed, _) =>
          checkExpr(indexing)
          val inPlaceModifiedElemType = exprMustBeArray(indexed.getType, tcCtx, varModif.getPosition, mustUpdate = true)
          val operatorRetType = mustExistOperator(inPlaceModifiedElemType, op, rhsType, varModif.getPosition)
          checkSubtypingConstraint(inPlaceModifiedElemType, operatorRetType, varModif.getPosition, "", tcCtx)
        case select@Select(structExpr, selected) =>
          val structType = checkExpr(structExpr)
          val inPlaceModifiedFieldType = exprMustHaveField(structExpr, selected, tcCtx, varModif.getPosition, mustUpdateField = true)
          val operatorRetType = mustExistOperator(inPlaceModifiedFieldType, op, rhsType, varModif.getPosition)
          checkSubtypingConstraint(inPlaceModifiedFieldType, operatorRetType, varModif.getPosition, "", tcCtx)
          select.setType(inPlaceModifiedFieldType)
        case _ =>
          reportError("syntax error: only variables, struct fields and array elements can be assigned", varModif.getPosition)
      }

    case ifThenElse@IfThenElse(cond, thenBr, elseBrOpt) =>
      val condType = checkExpr(cond)
      checkSubtypingConstraint(BoolType, condType, ifThenElse.getPosition, "if condition", tcCtx)
      val smartCasts = detectSmartCasts(cond, tcCtx)
      ifThenElse.setSmartCasts(smartCasts)
      checkStat(thenBr)(using tcCtx.copyWithSmartCasts(smartCasts))
      elseBrOpt.foreach(checkStat)

    case whileLoop@WhileLoop(cond, body) =>
      val condType = checkExpr(cond)
      checkSubtypingConstraint(BoolType, condType, whileLoop.getPosition, "while condition", tcCtx)
      val smartCasts = detectSmartCasts(cond, tcCtx)
      checkStat(body)(using tcCtx.copyWithSmartCasts(smartCasts))

    case forLoop@ForLoop(initStats, cond, stepStats, body) =>
      val newCtx = tcCtx.copied
      initStats.foreach(checkStat(_)(using newCtx))
      val condType = checkExpr(cond)(using newCtx)
      checkSubtypingConstraint(BoolType, condType, forLoop.getPosition, "for loop condition", tcCtx)
      val smartCasts = detectSmartCasts(cond, tcCtx)
      val smartCastsAwareCtx = newCtx.copyWithSmartCasts(smartCasts)
      stepStats.foreach(checkStat(_)(using smartCastsAwareCtx))
      checkStat(body)(using smartCastsAwareCtx)
      newCtx.writeLocalsRelatedWarnings(errorReporter)

    case retStat@ReturnStat(valueOpt) =>
      valueOpt.foreach { value =>
        val retType = checkExpr(value)
        checkSubtypingConstraint(expRetType, retType, retStat.getPosition, "returned value", tcCtx)
      }
      if (expRetType == NothingType) {
        reportError(s"unexpected return in function returning $NothingType", retStat.getPosition)
      } else if (expRetType != VoidType && valueOpt.isEmpty) {
        reportError("expected an expression after return", retStat.getPosition)
      }

    case panicStat@PanicStat(msg) =>
      val msgType = checkExpr(msg)
      checkSubtypingConstraint(StringType, msgType, panicStat.getPosition, "panic", tcCtx)

    case EnclosedStat(capabilities, body) =>
      for (capability <- capabilities) {
        val capabilityType = checkExpr(capability)
        // TODO support devices, and maybe structs and arrays
        checkSubtypingConstraint(RegionType ^ CaptureSet.singletonOfRoot, capabilityType, capability.getPosition, "enclosure permission", tcCtx)
      }
      checkStat(body)
  }

  private def checkLiteralExpr(literal: Literal): Type = literal match {
    case _: IntLit => IntType
    case _: DoubleLit => DoubleType
    case _: CharLit => CharType
    case _: BoolLit => BoolType
    case _: StringLit => StringType
  }

  private def checkExpr(expr: Expr)(using tcCtx: TypeCheckingContext, env: Environment): Type = {
    val tpe = expr match {

      case literal: Literal =>
        checkLiteralExpr(literal)

      case varRef@VariableRef(name) =>
        tcCtx.localIsQueried(name)
        tcCtx.getLocalOrConst(name) match
          case None =>
            reportError(s"unknown: $name", varRef.getPosition)
          case Some(localInfo: LocalInfo) =>
            localInfo.tpe

      case MeRef() =>
        tcCtx.meType

      case pkg@PackageRef(packageName) =>
        if (!env.allowsPackage(packageName)) {
          reportError(s"illegal reference to $packageName, which is not imported in this module", pkg.getPosition)
        }
        tcCtx.resolveType(packageName) match {
          case Some(packageSignature: PackageSignature) => NamedTypeShape(packageName)
          case Some(_) => reportError(s"$packageName is not a package and is thus not allowed here", pkg.getPosition)
          case None => reportError(s"not found: $packageName", pkg.getPosition)
        }

      case devRef@DeviceRef(device) =>
        if (!env.allowsDevice(device)) {
          reportError(s"illegal reference to device $device, which is not imported in this module", devRef.getPosition)
        }
        NamedTypeShape(device.sig.id)

      case call@Call(None, funName, args) =>
        checkFunCall(call, IntrinsicsPackageId, fallbackOwnerOpt = Some(tcCtx.meId))

      case call@Call(Some(receiver), funName, args) =>
        checkExpr(receiver).shape match {
          case namedType: NamedTypeShape =>
            checkFunCall(call, namedType.typeName, None)
          case recType =>
            reportError(s"expected a module or package type, found $recType", receiver.getPosition)
        }

      case indexing@Indexing(indexed, arg) =>
        val indexedType = checkExpr(indexed)
        val argType = checkExpr(arg)
        checkSubtypingConstraint(IntType, argType, indexing.getPosition, "array index", tcCtx)
        exprMustBeArray(indexed.getType, tcCtx, indexed.getPosition, mustUpdate = false)

      case arrayInit@ArrayInit(region, elemTypeTree, size) =>
        checkAndRequireRegion(region, tcCtx)
        if (elemTypeTree == VoidType || elemTypeTree == NothingType) {
          reportError(s"array cannot have element type $elemTypeTree", arrayInit.getPosition)
        }
        val sizeType = checkExpr(size)
        checkSubtypingConstraint(IntType, sizeType, size.getPosition, "array size", tcCtx)
        size match {
          case IntLit(value) if value < 0 =>
            reportError("array size should be nonnegative", size.getPosition)
          case _ => ()
        }
        val elemType = checkType(elemTypeTree)
        ArrayTypeShape(elemType, modifiable = true) ^ computeCaptures(List(region))

      case filledArrayInit@FilledArrayInit(regionOpt, Nil) =>
        regionOpt.foreach(checkAndRequireRegion(_, tcCtx))
        reportError("cannot infer type of empty array, use 'arr <type>[0]' instead", filledArrayInit.getPosition)

      case filledArrayInit@FilledArrayInit(regionOpt, arrayElems) =>
        regionOpt.foreach(checkAndRequireRegion(_, tcCtx))
        val types = arrayElems.map(checkExpr)
        computeJoinOf(types.toSet, tcCtx) match {
          case Some(elemsJoin) =>
            ArrayTypeShape(elemsJoin, modifiable = regionOpt.isDefined) ^ computeCaptures(regionOpt.toList)
          case None =>
            reportError("cannot infer array type", filledArrayInit.getPosition)
        }

      case instantiation@StructOrModuleInstantiation(regionOpt, tid, args) =>
        regionOpt.foreach(checkAndRequireRegion(_, tcCtx))
        tcCtx.resolveType(tid) match {
          case Some(structSig: StructSignature) if structSig.isInterface =>
            reportError(
              s"cannot instantiate interface $tid",
              instantiation.getPosition
            )
          case Some(structSig: StructSignature) =>
            if (structSig.isShallowMutable && regionOpt.isEmpty) {
              reportError(
                s"cannot instantiate '$tid' without providing a region, since at least one of its fields is reassignable",
                instantiation.getPosition
              )
            }
            checkCallArgs(structSig.constructorSig, None, args, tcCtx, instantiation.getPosition)
            // FIXME should we replace the me reference here? recursive type?
            NamedTypeShape(tid) ^ computeCaptures(args ++ regionOpt)
          case Some(moduleSig: ModuleSignature) =>
            checkCallArgs(moduleSig.constructorSig, None, args, tcCtx, instantiation.getPosition)
            // FIXME should we replace the me reference here? recursive type?
            NamedTypeShape(tid) ^ computeCaptures(args ++ regionOpt)
          case _ => reportError(s"not found: structure or module '$tid'", instantiation.getPosition)
        }

      case RegionCreation() =>
        RegionType ^ CaptureSet.singletonOfRoot

      case unaryOp@UnaryOp(operator, operand) =>
        val operandType = checkExpr(operand)
        val operandTypeShape = operandType.shape
        if (operator == Sharp) {
          if (operandTypeShape.isInstanceOf[ArrayTypeShape] || operandTypeShape == StringType) {
            IntType
          } else {
            reportError(s"operator # can only be applied to arrays and strings, found '$operandType'", unaryOp.getPosition)
          }
        } else {
          unaryOperatorSignatureFor(operator, operandType.shape) match {
            case Some(sig) => sig.retType
            case None =>
              reportError(s"no definition of operator '$operator' found for operand '$operandType'", unaryOp.getPosition)
          }
        }

      case binOp@BinaryOp(lhs, operator, rhs) =>
        // no check for unused mut because no binary operator requires mutability on its arguments
        val lhsType = checkExpr(lhs)
        val smartCasts = if operator == Operator.And then detectSmartCasts(lhs, tcCtx) else Map.empty
        val rhsType = checkExpr(rhs)(using tcCtx.copyWithSmartCasts(smartCasts), env)
        binOp.setSmartCasts(smartCasts)
        if (operator == Equality || operator == Inequality) {
          val isSubOrSupertype = lhsType.subtypeOf(rhsType) || rhsType.subtypeOf(lhsType)
          if (!isSubOrSupertype) {
            reportError(s"cannot compare '$lhsType' and '$rhsType' using ${Equality.str} or ${Inequality.str}", binOp.getPosition)
          }
          BoolType
        } else {
          mustExistOperator(lhsType, operator, rhsType, binOp.getPosition)
        }

      case select@Select(lhs, selected) =>
        val lhsType = checkExpr(lhs)
        exprMustHaveField(lhs, selected, tcCtx, select.getPosition, mustUpdateField = false)

      case ternary@Ternary(cond, thenBr, elseBr) =>
        val condType = checkExpr(cond)
        checkSubtypingConstraint(BoolType, condType, ternary.getPosition, "ternary operator condition", tcCtx)
        val smartCasts = detectSmartCasts(cond, tcCtx)
        ternary.setSmartCasts(smartCasts)
        val thenType = checkExpr(thenBr)(using tcCtx.copyWithSmartCasts(smartCasts), env)
        val elseType = checkExpr(elseBr)
        val thenIsSupertype = elseType.subtypeOf(thenType)
        val thenIsSubtype = thenType.subtypeOf(elseType)
        computeJoinOf(Set(thenType, elseType), tcCtx) match {
          case Some(ternaryType) =>
            ternaryType
          case None =>
            reportError(s"cannot infer type of ternary operator: then branch has type '$thenType', " +
              s"else branch has type '$elseType'", ternary.getPosition)
        }

      case cast@Cast(expr, typeTree) =>
        val exprType = checkExpr(expr)
        val targetType = checkCastTargetTypeShape(typeTree)
        val castTypeShape = if (exprType.shape.subtypeOf(targetType)) {
          reportError(s"useless conversion: '${exprType.shape}' --> '$targetType'", cast.getPosition, isWarning = true)
          cast.markTransparent()
          targetType
        } else if (TypeConversion.conversionFor(exprType.shape, targetType).isDefined) {
          targetType
        } else {
          structCastResult(exprType.shape, targetType, tcCtx).getOrElse {
            reportError(s"cannot cast '${expr.getType}' to '$targetType'", cast.getPosition)
          }
        }
        castTypeShape ^ exprType.captureDescriptor

      case typeTest@TypeTest(expr, typeTree) =>
        val exprType = checkExpr(expr)
        val targetType = checkCastTargetTypeShape(typeTree)
        if (exprType.shape.subtypeOf(targetType)) {
          reportError(s"test will always be true, as '${exprType.shape}' is a subtype of '$targetType'",
            typeTest.getPosition, isWarning = true)
        } else if (structCastResult(exprType.shape, targetType, tcCtx).isEmpty) {
          reportError(s"cannot check expression of type '${exprType.shape}' against type '$targetType'", typeTest.getPosition)
        }
        BoolType

      case _: Sequence => throw AssertionError("should not happen, as Sequences are produced by the desugaring phase")
    }
    expr.setType(tpe)
    tpe
  }

  private def checkAndRequireRegion(region: Expr, ctx: TypeCheckingContext)(using TypeCheckingContext, Environment): Unit = {
    val regType = checkExpr(region)
    checkSubtypingConstraint(PrimitiveTypeShape.RegionType ^ CaptureSet.singletonOfRoot, regType,
      region.getPosition, "region", ctx)
  }

  private def unaryOperatorSignatureFor(operator: Operator, operand: TypeShape)
                                       (using TypeCheckingContext, Environment): Option[UnaryOpSignature] = {
    unaryOperators.find {
      case UnaryOpSignature(op, operandType, _) =>
        operator == op && operand.subtypeOf(operandType)
    }
  }

  private def binaryOperatorSigFor(left: TypeShape, operator: Operator, right: TypeShape)
                                  (using TypeCheckingContext, Environment): Option[BinaryOpSignature] = {
    binaryOperators.find {
      case BinaryOpSignature(leftOperandType, op, rightOperandType, _) =>
        left.subtypeOf(leftOperandType) && op == operator && right.subtypeOf(rightOperandType)
    }
  }

  /**
   * @return (meType, callType)
   */
  private def checkFunCall(
                            call: Call,
                            owner: TypeIdentifier,
                            fallbackOwnerOpt: Option[TypeIdentifier]
                          )(using tcCtx: TypeCheckingContext, envir: Environment): Type = {
    val funName = call.function
    val args = call.args
    val pos = call.getPosition
    tcCtx.resolveFunc(owner, funName) match {
      case FunctionFound(funSig) =>
        call.setResolvedSig(funSig)
        call.cacheMeType(tcCtx.meType)
        checkCallArgs(funSig, call.receiverOpt, args, tcCtx, pos)
      case ModuleNotFound =>
        args.foreach(checkExpr)
        reportError(s"not found: package or module $owner", pos)
      case FunctionNotFound =>
        fallbackOwnerOpt map { fallbackOwner =>
          checkFunCall(call, fallbackOwner, None)
        } getOrElse {
          args.foreach(checkExpr)
          reportError(s"function not found: '$funName'", pos)
        }
    }
  }

  private def structCastResult(srcType: TypeShape, destType: TypeShape, ctx: TypeCheckingContext)
                              (using TypeCheckingContext, Environment): Option[NamedTypeShape] = {
    (srcType, destType) match {
      case (srcType: NamedTypeShape, destType: NamedTypeShape)
        if destType.subtypeOf(srcType) || srcType.subtypeOf(destType)
      => Some(destType)
      case _ => None
    }
  }

  private def checkCallArgs(funSig: FunctionSignature, receiverOpt: Option[Expr], args: List[Expr], ctx: TypeCheckingContext, callPos: Option[Position])
                           (using TypeCheckingContext, Environment): Type = {
    val expTypesIter = funSig.args.iterator
    val argsIter = args.iterator
    val substMap = mutable.Map.empty[Capturable, Capturable]
    saveArgMappingIfPath(substMap, Some(MePath), receiverOpt)
    val substitutor = CapturesSubstitutor.fromView(substMap)
    var errorFound = false
    while (expTypesIter.hasNext && argsIter.hasNext && !errorFound) {
      val (paramNameOpt, expTypeRaw) = expTypesIter.next()
      val expTypeSubst = substitutor.subst(expTypeRaw)
      val arg = argsIter.next()
      val actType = checkExpr(arg)
      if (!actType.subtypeOf(expTypeSubst)) {
        reportError(s"expected '$expTypeSubst', found '$actType'", arg.getPosition)
        errorFound = true
      }
      saveArgMappingIfPath(substMap, paramNameOpt.map(IdPath(_)), Some(arg))
    }
    if (expTypesIter.hasNext && !errorFound) {
      reportError(s"expected argument of type '${expTypesIter.next()._2}', found end of arguments list", callPos)
    }
    if (argsIter.hasNext && !errorFound) {
      val arg = argsIter.next()
      reportError(s"expected end of arguments list, found argument of type '${checkExpr(arg)}'", arg.getPosition)
    }
    for arg <- argsIter do {
      checkExpr(arg)
    }
    substitutor.subst(funSig.retType)
  }

  private def computeCaptures(captured: List[Expr]): CaptureDescriptor =
    CaptureDescriptors.unionOf(captured.map(minimalCaptureSetFor))

  private def saveArgMappingIfPath(
                                    substMap: mutable.Map[Capturable, Capturable],
                                    paramOpt: Option[Capturable],
                                    argOpt: Option[Expr]
                                  ): Unit = {
    for {
      param <- paramOpt
      arg <- argOpt
      argPath <- PathsConverter.convertOrFailSilently(arg)
    } do {
      substMap(param) = argPath
    }
  }

  private def detectSmartCasts(expr: Expr, ctx: TypeCheckingContext): Map[FunOrVarId, CastTargetTypeShape] = {
    expr match {
      case BinaryOp(lhs, Operator.And, rhs) =>
        // concat order is reversed because we want to keep the leftmost type test in case of conflict
        detectSmartCasts(rhs, ctx) ++ detectSmartCasts(lhs, ctx)
      case TypeTest(VariableRef(varName), typeTree) if ctx.getLocalOnly(varName).exists(!_.isReassignable) =>
        typeTree.getResolvedTypeOpt.map { tpe =>
          Map(varName -> tpe)
        }.getOrElse(Map.empty)
      case _ => Map.empty
    }
  }

  /**
   * @param returned      types of all the expressions found after a `return`
   * @param alwaysStopped indicates whether the control-flow can reach the end of the considered construct without
   *                      encountering an instruction that terminates the function (`return` or `panic`)
   */
  private case class EndStatus(alwaysStopped: Boolean)

  /**
   * Traverses the program, looking for instructions that end the function they are in (`return` and `panic`)
   */
  private def checkReturns(ast: Ast): EndStatus = {
    ast match {

      case Source(defs) =>
        for df <- defs do {
          checkReturns(df)
        }
        EndStatus(false)

      case Block(stats) =>
        var alreadyReportedDeadCode = false
        var endStatus = EndStatus(false)
        for stat <- stats do {
          if (endStatus.alwaysStopped && !alreadyReportedDeadCode) {
            reportError("dead code", stat.getPosition)
            alreadyReportedDeadCode = true
          }
          val statEndStatus = checkReturns(stat)
          endStatus = EndStatus(endStatus.alwaysStopped || statEndStatus.alwaysStopped)
        }
        endStatus

      case ifThenElse@IfThenElse(_, thenBr, elseBrOpt) =>
        val thenEndStatus = checkReturns(thenBr)
        val elseEndStatus = elseBrOpt.map(checkReturns).getOrElse(EndStatus(alwaysStopped = false))
        EndStatus(thenEndStatus.alwaysStopped && elseEndStatus.alwaysStopped)

      case _: Ternary => EndStatus(false)

      case whileLoop@WhileLoop(_, body) =>
        val bodyEndStatus = checkReturns(body)
        if (bodyEndStatus.alwaysStopped) {
          reportError("while should be replaced by if", whileLoop.getPosition, isWarning = true)
        }
        bodyEndStatus

      case forLoop@ForLoop(_, _, _, body) =>
        val bodyEndStatus = checkReturns(body)
        if (bodyEndStatus.alwaysStopped) {
          reportError("for should be replaced by if", forLoop.getPosition, isWarning = true)
        }
        bodyEndStatus

      case retStat: ReturnStat =>
        val retType = retStat.getRetType
        if (retType.isEmpty) {
          reportError("could not infer type of returned value", retStat.getPosition)
        }
        EndStatus(true)

      case _: PanicStat =>
        EndStatus(true)

      case EnclosedStat(_, body) =>
        checkReturns(body)

      case _: (FunDef | StructDef | Param) =>
        assert(false)

      case _ => EndStatus(false)

    }
  }

  private def mustBeReassignable(name: FunOrVarId, ctx: TypeCheckingContext, posOpt: Option[Position]): Type = {
    ctx.getLocalOrConst(name) match
      case None =>
        reportError(s"unknown: $name", posOpt)
      case Some(LocalInfo(_, tpe, isReassignable, _, _, _)) =>
        if (!isReassignable) {
          reportError(s"$name is not reassignable", posOpt)
        }
        tpe
  }

  private def exprMustBeArray(exprType: Type, ctx: TypeCheckingContext, posOpt: Option[Position], mustUpdate: Boolean): Type = {
    exprType.shape match
      case ArrayTypeShape(elemType, modifiable) =>
        if (mustUpdate && !modifiable) {
          reportError("update impossible: missing modification privileges on array", posOpt)
        }
        elemType
      case _ =>
        reportError(s"expected an array, found '$exprType'", posOpt)
  }

  private def mustExistOperator(lhsType: Type, operator: Operator, rhsType: Type, position: Option[Position])
                               (using TypeCheckingContext, Environment): Type = {
    binaryOperatorSigFor(lhsType.shape, operator, rhsType.shape) match {
      case Some(sig) => sig.retType
      case None =>
        reportError(s"no definition of operator '$operator' found for operands '$lhsType' and '$rhsType'", position)
    }
  }

  private def exprMustHaveField(
                                 expr: Expr,
                                 fieldName: FunOrVarId,
                                 ctx: TypeCheckingContext,
                                 posOpt: Option[Position],
                                 mustUpdateField: Boolean
                               ): Type = {
    val exprType = expr.getType
    exprType.shape match {
      case NamedTypeShape(typeName) =>
        ctx.resolveType(typeName) match {
          case Some(StructSignature(_, fields, _, _)) if fields.contains(fieldName) =>
            val FieldInfo(fieldType, fieldIsReassig) = fields.apply(fieldName)
            val missingFieldMutability = mustUpdateField && !fieldIsReassig
            if (missingFieldMutability) {
              reportError(s"cannot update immutable field '$fieldName'", posOpt)
            }
            fieldType
          case Some(ModuleSignature(_, paramImports, _, _, _)) if paramImports.contains(fieldName) =>
            if (!expr.isInstanceOf[MeRef]) {
              reportError(s"references to module imports are only allowed when using the '${Keyword.Me}' keyword", posOpt)
            }
            if (mustUpdateField) {
              reportError("cannot update module field", posOpt)
            }
            paramImports.apply(fieldName)
          case _ =>
            reportError(s"'$exprType' has no field named '$fieldName'", posOpt)
        }
      case _ =>
        reportError(s"'$exprType' has no field named '$fieldName'", posOpt)
    }
  }

  private def checkSubtypingConstraint(
                                        expected: Type,
                                        actual: Type,
                                        posOpt: Option[Position],
                                        msgPrefix: String,
                                        ctx: TypeCheckingContext
                                      )(using TypeCheckingContext, Environment): Unit = {
    if (expected != UndefinedTypeShape && actual != UndefinedTypeShape && !actual.subtypeOf(expected)) {
      val fullprefix = if msgPrefix == "" then "" else (msgPrefix ++ ": ")
      reportError(fullprefix ++ s"expected '$expected', found '$actual'", posOpt)
    }
  }

  private def computeJoinOf(types: Set[Type], ctx: TypeCheckingContext): Option[Type] = {
    require(types.nonEmpty)
    if types.size == 1 then Some(types.head)
    else if (areAllStructs(types, ctx)) {
      val structs = ctx.structs
      // if the structs have exactly 1 direct supertype in common, infer it as the result type
      val directSupertypesSets = types.map { tpe =>
        structs.apply(tpe.shape.asInstanceOf[NamedTypeShape].typeName).directSupertypes.toSet
      }
      val commonDirectSupertypes = directSupertypesSets.reduceLeft(_.intersect(_))
      Some(
        if commonDirectSupertypes.size == 1
        then {
          val superT = commonDirectSupertypes.head
          NamedTypeShape(superT)
        } else {
          val cd = CaptureDescriptors.unionOf(types.map(_.captureDescriptor))
          UnionTypeShape(types.map(_.shape)) ^ cd
        }
      )
    } else {
      // type Nothing should be accepted
      // e.g. in 'when ... then 0 else (panic "")'
      types.find { commonSuper =>
        types.forall(_.subtypeOf(commonSuper)(using ctx))
      }
    }
  }

  private def areAllStructs(types: Set[Type], ctx: TypeCheckingContext): Boolean = {
    types.forall {
      case NamedTypeShape(typeName) => ctx.resolveType(typeName).exists(_.isInstanceOf[StructSignature])
      case _ => false
    }
  }

  private def minimalCaptureSetFor(expr: Expr): CaptureDescriptor = {
    PathsConverter.convertOrFailSilently(expr) map { path =>
      CaptureSet(path)
    } getOrElse {
      expr.getType.captureDescriptor
    }
  }

  private def reportError(msg: String, pos: Option[Position], isWarning: Boolean = false): UndefinedTypeShape.type = {
    errorReporter.push(if isWarning then Warning(TypeChecking, msg, pos) else Err(TypeChecking, msg, pos))
    UndefinedTypeShape
  }

  private def logicalImplies(p: Boolean, q: Boolean): Boolean = !p || q

  private def shouldNotHappen(): Nothing = {
    throw new AssertionError("should not happen")
  }

}
