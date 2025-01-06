package compiler

import java.io.File
import java.nio.file.Path

object TestRuntimePaths {

  private val rattlesnakeRootDir =
    new File("")
      .getCanonicalFile
      .getParentFile
      .toPath

  val runtimeTargetDirPath: Path =
    rattlesnakeRootDir
      .resolve("rattlesnake-runtime")
      .resolve("target")

  val agentTargetDirPath: Path =
    rattlesnakeRootDir
      .resolve("rattlesnake-agent")
      .resolve("target")
  
}
