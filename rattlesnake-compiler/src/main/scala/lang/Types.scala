package lang

import identifiers.TypeIdentifier
import lang.CaptureDescriptors.*

import scala.annotation.targetName


object Types {

  sealed trait Type {
    def shape: TypeShape
    def captureDescriptor: CaptureDescriptor
    
    def isPure: Boolean = captureDescriptor.isEmpty

    def markedIfNeeded: Type
    def cdErased: TypeShape

    def propagateMarkOf(maybeMarked: Type): Type = {
      if maybeMarked.captureDescriptor == Mark then this.markedIfNeeded else this
    }
  }

  final case class CapturingType private[CapturingType](shape: TypeShape, captureDescriptor: CaptureDescriptor) extends Type {
    require(!captureDescriptor.isEmpty)

    override def markedIfNeeded: Type = shape.markedIfNeeded
    override def cdErased: TypeShape = shape.cdErased

    override def toString: String =
      if captureDescriptor.isEmpty then shape.toString
      else if captureDescriptor.coversRoot then shape.toStringCapturing("")
      else shape.toStringCapturing(captureDescriptor.toString)
  }
  
  object CapturingType {
    def apply(shape: Types.TypeShape, descriptor: CaptureDescriptor): Type = {
      if descriptor.isEmpty then shape else new CapturingType(shape, descriptor)
    }
  }

  sealed trait TypeShape extends Type {
    override def shape: TypeShape = this
    override def captureDescriptor: CaptureDescriptor = CaptureSet.empty
    @targetName("capturing") infix def ^(cd: CaptureDescriptor): Type = CapturingType(this, cd)
    @targetName("maybeCapturing") infix def ^(cdOpt: Option[CaptureDescriptor]): Type =
      cdOpt.map(CapturingType(this, _)).getOrElse(this)
    def mayCapture: Boolean
    private[Types] def toStringCapturing(capDescrStr: String): String = s"$toString^$capDescrStr"
  }

  sealed trait CastTargetTypeShape extends TypeShape

  enum PrimitiveTypeShape(val str: String, override val mayCapture: Boolean) extends CastTargetTypeShape {
    case IntType extends PrimitiveTypeShape("Int", false)
    case DoubleType extends PrimitiveTypeShape("Double", false)
    case CharType extends PrimitiveTypeShape("Char", false)
    case BoolType extends PrimitiveTypeShape("Bool", false)
    case StringType extends PrimitiveTypeShape("String", false)
    case RegionType extends PrimitiveTypeShape("Region", true)

    case VoidType extends PrimitiveTypeShape("Void", false)
    case NothingType extends PrimitiveTypeShape("Nothing", false)

    override def markedIfNeeded: Type = this match {
      case RegionType => RegionType ^ Mark
      case _ => this
    }

    override def cdErased: TypeShape = this

    override def toString: String = str
  }

  def primTypeFor(name: TypeIdentifier): Option[PrimitiveTypeShape] = {
    PrimitiveTypeShape.values.find(_.str == name.stringId)
  }

  final case class NamedTypeShape(typeName: TypeIdentifier) extends CastTargetTypeShape {
    override def mayCapture: Boolean = true

    override def markedIfNeeded: Type = this ^ Mark
    override def cdErased: TypeShape = this

    override def toString: String = typeName.stringId
  }

  /**
   * Type of an array
   * @param elemType type of array elements
   */
  final case class ArrayTypeShape(elemType: Type) extends TypeShape {
    override def mayCapture: Boolean = true

    override def markedIfNeeded: Type = ArrayTypeShape(elemType.cdErased) ^ Mark
    override def cdErased: TypeShape = ArrayTypeShape(elemType.cdErased)

    override def toString: String = {
      s"${Keyword.Arr.str} $elemType"
    }

    private[Types] override def toStringCapturing(capDescrStr: String): String =
      s"${Keyword.Arr.str}^$capDescrStr $elemType"
  }

  final case class UnionTypeShape(unitedTypes: Set[TypeShape]) extends TypeShape {
    override def mayCapture: Boolean = unitedTypes.exists(_.mayCapture)
    override def cdErased: TypeShape = UnionTypeShape(unitedTypes.map(_.cdErased))

    override def markedIfNeeded: Type = if mayCapture then this ^ Mark else this

    override def toString: String = unitedTypes.toSeq.sortBy(_.toString).mkString(" | ")
  }

  /**
   * Type of a malformed/incorrect expression
   */
  case object UndefinedTypeShape extends TypeShape {
    override def mayCapture: Boolean = false

    override def markedIfNeeded: Type = this
    override def cdErased: TypeShape = this

    override def toString: String = "[undefined type]"
  }

}
