package compiler

import compiler.ExecutionTests.{rootPathStr, stderrFileName, stdoutFileName, testOutSubdirName}
import compiler.gennames.ClassesAndDirectoriesNames.outDirName
import compiler.gennames.{ClassesAndDirectoriesNames, FileExtensions}
import compiler.io.SourceFile
import compiler.pipeline.TasksPipelines
import compiler.reporting.Errors.ErrorReporter
import compiler.runners.{MainFinder, Runner}
import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import org.objectweb.asm.Opcodes.V1_8

import java.io.InputStream
import java.nio.file.{Files, Path, Paths}
import scala.io.Source


object ExecutionTests {

  private val rootPathStr: String = "./src/test/res/execution-tests/"
  private val stdoutFileName: String = "stdout.exp"
  private val stderrFileName: String = "stderr.exp"
  private val testOutSubdirName: String = "testout"
  private val javaVersionCode = V1_8

  @Parameters(name = "{0}")
  def allDirectories(): java.lang.Iterable[Array[String]] = {
    Files.list(Paths.get(rootPathStr)).map(filePath => Array(filePath.getFileName.toString)).toList
  }

}

@RunWith(classOf[Parameterized])
class ExecutionTests(programDirName: String) {
  import ExecutionTests.javaVersionCode
  
  @Test
  def runProgramTest(): Unit = {

    val programDirPath = Path.of(rootPathStr).resolve(programDirName)
    val expOut = getExpectedData(programDirPath.resolve(stdoutFileName))

    object ExitException extends RuntimeException

    val er = ErrorReporter(System.err.println, exit = throw ExitException)
    val srcFiles = getAllSourcesInProgram(programDirPath).map(s => SourceFile(programDirPath.resolve(s).toString))
    val testOutSubdirPath = programDirPath.resolve(testOutSubdirName)
    val writtenFilePaths = TasksPipelines
      .compiler(testOutSubdirPath, javaVersionCode, er)
      .apply(srcFiles)

    def errorCallback(msg: String): Nothing = {
      throw AssertionError(msg)
    }

    val mainClassName = MainFinder.findMainClassNameAmong(writtenFilePaths).fold(throw _, identity)
    val process =
      Runner(errorCallback, testOutSubdirPath.resolve(outDirName))
        .runMain(mainClassName, inheritIO = false)

    val exitCode = process.waitFor()
    val actOut = readProgramStream(process.getInputStream)
    val actErr = readProgramStream(process.getErrorStream)

    assertEquals(s"unexpected output on stdout (exit code: $exitCode, stderr:\n$actErr\n)", expOut, actOut)
    assertEquals(s"non-zero exit code, stderr: $actErr", 0, exitCode)
  }

  private def readProgramStream(is: InputStream): String = {
    Source.fromInputStream(is)
      .mkString
      .lines()
      .toArray(new Array[String](_))
      .mkString("\n")
  }

  private def getExpectedData(path: Path): String = {
    Files.readAllLines(path).toArray.mkString("\n")
  }

  private def getAllSourcesInProgram(dirPath: Path): List[String] = {
    dirPath.toFile.list((_, name) => name.endsWith(FileExtensions.rattlesnake)).toList
  }
  
}
