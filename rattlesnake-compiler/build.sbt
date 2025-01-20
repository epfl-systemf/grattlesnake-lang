name := "Rattlesnake"

version := "0.2.0-SNAPSHOT"

scalaVersion := "3.5.0"
javacOptions ++= Seq("-source", "21", "-target", "21")

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
libraryDependencies += "org.ow2.asm" % "asm" % "9.3"
libraryDependencies += "org.ow2.asm" % "asm-util" % "9.3"

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}
