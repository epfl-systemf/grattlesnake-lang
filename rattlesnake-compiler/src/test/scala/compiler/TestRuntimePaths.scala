package compiler

import java.io.File
import java.nio.file.Path

object TestRuntimePaths {

  private val rattlesnakeRootDir =
    new File("")
      .getCanonicalFile
      .getParentFile
      .toPath

  val jarsDir: Path = rattlesnakeRootDir.resolve("jars")
  
}
