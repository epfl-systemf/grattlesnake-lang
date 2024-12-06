package lang

enum Keyword(val str: String) {
  case Arr extends Keyword("arr")
  case As extends Keyword("as")
  case Cap extends Keyword("cap")
  case Const extends Keyword("const")
  case Device extends Keyword("device")
  case Else extends Keyword("else")
  case Enclosed extends Keyword("enclosed")
  case Fn extends Keyword("fn")
  case For extends Keyword("for")
  case Fs extends Keyword("fs")
  case If extends Keyword("if")
  case Interface extends Keyword("interface")
  case Is extends Keyword("is")
  case Me extends Keyword("me")
  case Module extends Keyword("module")
  case Mut extends Keyword("mut")
  case New extends Keyword("new")
  case NewRegion extends Keyword("newregion")
  case Package extends Keyword("package")
  case Panic extends Keyword("panic")
  case Restricted extends Keyword("restricted")
  case Return extends Keyword("return")
  case Struct extends Keyword("struct")
  case Test extends Keyword("test")
  case Then extends Keyword("then")
  case Val extends Keyword("val")
  case Var extends Keyword("var")
  case When extends Keyword("when")
  case While extends Keyword("while")

  override def toString: String = str
}
