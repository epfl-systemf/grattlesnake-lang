package compiler.typechecker

import compiler.irs.Asts.*
import compiler.pipeline.CompilationStep.TypeChecking
import compiler.reporting.Errors.{Err, ErrorReporter}
import lang.Capturables.*
import lang.{ConstructibleSig, SelectableSig}
import lang.Types.*

object CapturablesConverter {

  def convertToCapturable(expr: Expr, erOpt: Option[ErrorReporter], idsAreFields: Boolean)
                                 (using tcCtx: TypeCheckingContext): Option[ConcreteCapturable] = {

    def maybeReportError(msg: String): None.type = {
      erOpt.foreach(_.push(Err(TypeChecking, msg, expr.getPosition)))
      None
    }

    expr match {
      case VariableRef(name) =>
        tcCtx.getLocalOnly(name) match {
          case None => None // do not report, an error will be reported checkExpr anyway
          case Some(localInfo) if localInfo.isReassignable =>
            maybeReportError(s"'$name' is not capturable, as it is a var")
          case Some(localInfo) if idsAreFields =>
            Some(MePath.dot(name))
          case Some(localInfo) =>
            Some(IdPath(name))
        }
      case MeRef() => Some(MePath)
      case PackageRef(pkgName) => Some(CapPackage(pkgName))
      case DeviceRef(device) => Some(CapDevice(device))
      case Select(lhs, selected) =>
        convertToCapturable(lhs, erOpt, idsAreFields).flatMap {
          case lhsPath: Path => {
            tcCtx.lookup(lhsPath).shape match {
              case NamedTypeShape(lhsTypeId) =>
                tcCtx.resolveTypeAs[SelectableSig & ConstructibleSig](lhsTypeId).flatMap { lhsTypeSig =>
                  lhsTypeSig.params.get(selected).flatMap { fieldInfo =>
                    if fieldInfo.isReassignable
                    then maybeReportError(s"field $selected of $lhsTypeId is not capturable, as it is reassignable")
                    else Some(lhsPath.dot(selected))
                  }
                }
              case ArrayTypeShape(elemType) =>
                Some(lhsPath.dot(selected))
              case lhsType => None
            }
          }
          case _ => None
        }
      case _ => maybeReportError(s"expression is not capturable")
    }
  }
  
}
