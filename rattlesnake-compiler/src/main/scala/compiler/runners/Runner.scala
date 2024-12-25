package compiler.runners

import compiler.gennames.ClassesAndDirectoriesNames.agentSubdirName

import java.io.File
import java.nio.file.Path

final class Runner(errorCallback: String => Nothing, workingDirectoryPath: Path) {

  private val classPathsSep =
    if System.getProperty("os.name").startsWith("Windows")
    then ";"
    else ":"

  def runMain(mainClassName: String, inheritIO: Boolean): Process = {
    val agentSubdirPath = workingDirectoryPath.resolve(agentSubdirName)
    val agentJarName = findNameOfJarInDir(agentSubdirPath.toFile, "Rattlesnake-agent",
      "Rattlesnake agent not found")
    val runtimeJarName = findNameOfJarInDir(workingDirectoryPath.toFile, "Rattlesnake-runtime",
      "Rattlesnake runtime not found")
    val agentJarFullPath = agentSubdirPath.resolve(agentJarName).toFile.getCanonicalFile
    val runtimeJarFullPath = workingDirectoryPath.resolve(runtimeJarName).toFile.getCanonicalFile
    val processBuilder = new ProcessBuilder()
      .directory(workingDirectoryPath.toFile)
      .command(
        "java",
        "-cp", s"$runtimeJarFullPath$classPathsSep.",
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
