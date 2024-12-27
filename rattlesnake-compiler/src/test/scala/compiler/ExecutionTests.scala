package compiler

import compiler.ExecutionTests.*
import compiler.gennames.FileExtensions
import compiler.io.SourceFile
import compiler.pipeline.TasksPipelines
import compiler.reporting.Errors.ErrorReporter
import compiler.runners.{MainFinder, Runner}
import org.junit.Assert.{assertEquals, assertNotEquals, assertTrue}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import org.objectweb.asm.Opcodes.V1_8

import java.io.InputStream
import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.util.Using


object ExecutionTests {

  private val rootPathStr: String = "./src/test/res/execution-tests/"
  private val stdoutFileName: String = "stdout.exp"
  private val stderrFileName: String = "stderr.exp"
  private val stacktraceFileName: String = "error-trace.exp"
  private val testOutSubdirName: String = "testout"
  private val stackTraceCommentMarker = "//>STACKTRACE"
  private val javaVersionCode = V1_8

  @Parameters(name = "{0}")
  def allDirectories(): java.lang.Iterable[Array[String]] = {
    Files.list(Paths.get(rootPathStr)).map(filePath => Array(filePath.getFileName.toString)).toList
  }

}

@RunWith(classOf[Parameterized])
class ExecutionTests(programDirName: String) {

  import ExecutionTests.javaVersionCode

  private val programDirPath = Path.of(rootPathStr).resolve(programDirName)

  @Test
  def runProgramTest(): Unit = {

    val expOut = getExpectedData(programDirPath.resolve(stdoutFileName))

    object ExitException extends RuntimeException

    val er = ErrorReporter(System.err.println, exit = throw ExitException)
    val srcFiles = getAllSourcesInProgram(programDirPath).map(s => SourceFile(programDirPath.resolve(s).toString))
    val testOutSubdirPath = programDirPath.resolve(testOutSubdirName)
    val writtenFilePaths =
      TasksPipelines
        .compiler(testOutSubdirPath, javaVersionCode, er)
        .apply(srcFiles)

    def errorCallback(msg: String): Nothing = {
      throw AssertionError(msg)
    }

    val mainClassName = MainFinder.findMainClassNameAmong(writtenFilePaths).fold(throw _, identity)
    val process = Runner(errorCallback, testOutSubdirPath).runMain(mainClassName, inheritIO = false)

    val exitCode = process.waitFor()
    val actOut = readProgramStream(process.getInputStream)
    val actErr = readProgramStream(process.getErrorStream)

    val msgOpt = maybeGetTraceMessage()
    val stacktraceEntries = getTraceEntries(srcFiles)
    assert(!(msgOpt.isDefined && stacktraceEntries.isEmpty),
      "no stacktrace entry provided for a test program that is expected to crash")
    assert(!(msgOpt.isEmpty && stacktraceEntries.nonEmpty),
      "test expects some lines to appear in a stacktrace but does not specify the error message")

    if (msgOpt.isDefined) {
      assertMatches(msgOpt.get, stacktraceEntries, actErr)
      assertNotEquals("expected an error, but got a zero exit code", 0, exitCode)
    } else {
      assertEquals(s"unexpected output on stdout (exit code: $exitCode)", expOut, actOut)
      assertEquals(s"non-zero exit code", 0, exitCode)
    }
  }

  private def readProgramStream(is: InputStream): String = {
    new String(is.readAllBytes())
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

  private def maybeGetTraceMessage(): Option[String] = {
    val traceFile = programDirPath.resolve(stacktraceFileName).toFile
    if (traceFile.exists()) {
      val lines = Using(Source.fromFile(traceFile))(_.getLines().mkString("\n")).get
      Some(lines)
    } else None
  }

  private def getTraceEntries(srcFiles: Seq[SourceFile]): Seq[(Filename, Int)] = {
    srcFiles.flatMap(getTraceEntries)
  }

  private def getTraceEntries(srcFile: SourceFile): Seq[(Filename, Int)] = {
    val srcFileName = nameFromPath(srcFile.name)
    val lines = srcFile.lines.get
    val entriesB = Seq.newBuilder[(Filename, Int)]
    var lineIdx = 1
    for line <- lines do {
      if (line.contains(stackTraceCommentMarker)) {
        entriesB.addOne((srcFileName, lineIdx))
      }
      lineIdx += 1
    }
    entriesB.result()
  }

  private type Filename = String

  private def assertMatches(expMsg: String, reqEntries: Seq[(Filename, Int)], actErr: String): Unit = {
    val lines = actErr.lines().toArray(new Array[String](_))
    val actMsg = lines.head.split(':')(1)
    val actualEntries = extractEntriesFromStacktrace(lines.tail)
    assertEquals(expMsg.trim, actMsg.trim)
    for (fn, l) <- reqEntries do {
      assertTrue(s"expected entry not found: $fn:$l",
        actualEntries.exists((afn, alo) => afn == fn && alo.contains(l)))
    }
  }

  private def extractEntriesFromStacktrace(lines: Array[String]): Array[(Filename, Option[Int])] = {
    lines.map { line =>
      val openParenthIdx = line.indexOf('(')
      val idxOfLastSep = line.lastIndexOf('/') max line.lastIndexOf('\\')
      val startIdx = (openParenthIdx max idxOfLastSep) + 1
      val fileAndLine = line.substring(startIdx, line.length - 1)
      fileAndLine.split(':') match {
        case Array(actualFileName) =>
          (nameFromPath(actualFileName), None)
        case Array(actualFileName, lineIdxStr) =>
          (nameFromPath(actualFileName), Some(lineIdxStr.toInt))
        case _ => assert(false)
      }
    }
  }

  private def nameFromPath(p: String): String = Path.of(p).getFileName.toString

}
