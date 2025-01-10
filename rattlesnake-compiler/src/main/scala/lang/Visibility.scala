package lang

enum Visibility {
  case Private, Public
  
  def isPublic: Boolean = this == Public
  
  def isPrivate: Boolean = this == Private
  
}
