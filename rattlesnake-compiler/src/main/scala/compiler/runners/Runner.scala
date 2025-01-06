package compiler.runners

import compiler.gennames.ClassesAndDirectoriesNames.{agentSubdirName, outDirName}

import java.io.File
import java.nio.file.Path

final class Runner(errorCallback: String => Nothing, workingDirectoryPath: Path) {

  private val classPathsSep =
    if System.getProperty("os.name").startsWith("Windows")
    then ";"
    else ":"

  def runMain(mainClassName: String, inheritIO: Boolean): Process = {
    val outDirPath = workingDirectoryPath.resolve(outDirName)
    val agentSubdirPath = outDirPath.resolve(agentSubdirName)
    val agentJarName = findNameOfJarInDir(agentSubdirPath.toFile, "rattlesnake-agent",
      "Rattlesnake agent not found")
    val runtimeJarName = findNameOfJarInDir(outDirPath.toFile, "rattlesnake-runtime",
      "Rattlesnake runtime not found")
    val agentJarFullPath = agentSubdirPath.resolve(agentJarName).toFile.getCanonicalFile
    val runtimeJarFullPath = outDirPath.resolve(runtimeJarName).toFile.getCanonicalFile
    val processBuilder = new ProcessBuilder()
      .directory(workingDirectoryPath.toFile)
      .command(
        "java",
        "-cp", s"$runtimeJarFullPath$classPathsSep./$outDirName",
        s"-javaagent:$agentJarFullPath",
        mainClassName
      )
    if (inheritIO) {
      processBuilder.inheritIO()
    }
    processBuilder.start()
  }

  private def findNameOfJarInDir(dir: File, jarNamePrefix: String, errorMsg: String): String = {
    dir.list().find(f => f.startsWith(jarNamePrefix) && f.endsWith("with-dependencies.jar"))
      .getOrElse {
        errorCallback(errorMsg)
      }
  }

}
