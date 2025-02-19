package lang

import lang.Capturables.*

object CaptureDescriptors {

  sealed trait CaptureDescriptor {

    def coversRoot: Boolean

    def isEmpty: Boolean

    def union(that: CaptureDescriptor): CaptureDescriptor = (this, that) match {
      case (thisCs: CaptureSet, thatCs: CaptureSet) => thisCs.union(thatCs)
      case _ => Mark
    }

    def union(thatOpt: Option[CaptureDescriptor]): CaptureDescriptor =
      thatOpt.map(this.union).getOrElse(this)

    def mapSet(f: Set[Capturable] => Set[Capturable]): CaptureDescriptor = this match {
      case Mark => Mark
      case CaptureSet(set) => CaptureSet(f(set))
    }

  }

  case object Mark extends CaptureDescriptor {
    override def coversRoot: Boolean = false

    override def isEmpty: Boolean = false

    override def toString: String = Operator.Sharp.str
  }

  final case class CaptureSet(set: Set[Capturable]) extends CaptureDescriptor {
    override def coversRoot: Boolean = set.contains(RootCapability)

    def augmentedWith(c: Capturable): CaptureSet = CaptureSet(set + c)
    def union(that: CaptureSet): CaptureSet = CaptureSet(this.set ++ that.set)

    override def isEmpty: Boolean = set.isEmpty

    override def toString: String =
      set.toList
        .map(_.toString)
        .sorted
        .mkString("{", ", ", "}")
  }

  object CaptureSet {

    def apply(values: Capturable*): CaptureSet = CaptureSet(Set(values *))

    val empty: CaptureSet = CaptureSet(Set.empty)
    val singletonOfRoot: CaptureSet = CaptureSet(RootCapability)

  }

  def unionOf(captureDescriptors: Iterable[CaptureDescriptor]): CaptureDescriptor = {
    captureDescriptors.foldLeft[CaptureDescriptor](CaptureSet.empty)(_.union(_))
  }

  val emptyCaptureSet: CaptureDescriptor = CaptureSet.empty
  val singletonSetOfRoot: CaptureDescriptor = CaptureSet.singletonOfRoot

}
