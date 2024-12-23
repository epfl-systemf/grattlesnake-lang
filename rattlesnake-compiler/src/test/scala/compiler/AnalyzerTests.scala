package compiler

import compiler.AnalyzerTests.*
import compiler.io.SourceFile
import compiler.pipeline.TasksPipelines
import compiler.reporting.Errors.*
import org.junit.Assert.fail
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters

import java.io.File
import java.nio.file.{Files, Paths}
import scala.collection.mutable

/**
 * Expected errors should be specified in formalized comments, starting with `//>`.
 * The comment should contain a code for the error level (E for error, W for warning), and the expected message, e.g.:
 * {{{ //> W : unused local: 'x' is never queried }}}
 * The error is expected to be reported at the line where the comment specifying it is positioned:
 * {{{ //> E@col=41 : expected 'S', found 'I' }}}
 * The error level code can be postfixed with `@col=` and the column index if you want to assert the column position as well.
 */
object AnalyzerTests {

  private val rootPathStr: String = "./src/test/res/analyzer-tests/"
  private val matcherMarker: String = "//>"
  private val matcherSep: String = "<//>"
  private val colMarker: String = "@col="

  @Parameters(name = "{0}")
  def allFiles(): java.lang.Iterable[Array[String]] = {
    Files.list(Paths.get(rootPathStr)).map(filePath => Array(filePath.getFileName.toString)).toList
  }

  private enum Level {
    case Error, Warning
  }

  private case class Matcher(
                              msg: String,
                              fileName: String,
                              oneBasedLineIdx: Int,
                              oneBasedColIdxOpt: Option[Int],
                              level: Level
                            ) {

    import Level.Warning

    def matches(err: CompilationError): Boolean = {
      err.msg.trim == msg.trim
        && err.posOpt.exists(_.srcCodeProviderName == fileName)
        && err.posOpt.exists(_.line == oneBasedLineIdx)
        && oneBasedColIdxOpt.forall(oneBasedColIdx => err.posOpt.exists(_.col == oneBasedColIdx))
        && (err.isWarning == (level == Warning))
    }

    override def toString: String = {
      val levelDescr = level.toString.toLowerCase
      val colDescr = oneBasedColIdxOpt.map(":" + _).getOrElse("")
      s"[$levelDescr] $fileName:$oneBasedLineIdx$colDescr : $msg"
    }

  }

}

@RunWith(classOf[Parameterized])
class AnalyzerTests(fileName: String) {

  @Test
  def checkFileTest(): Unit = {

    val testRootPathStr = rootPathStr + fileName
    val testRootFile = new File(testRootPathStr)
    val srcFileNames =
      if testRootFile.isDirectory
      then testRootFile.list().toList
      else List(testRootPathStr)
    val srcFiles = srcFileNames.map(SourceFile(_))

    val expectedErrors = mutable.LinkedHashMap.empty[Matcher, Boolean]
    for (srcFile <- srcFiles) {
      val matchers = readErrorsMatchers(srcFile.lines.get, srcFile.name)
      expectedErrors.addAll(matchers.map(_ -> false))
    }

    val errors = mutable.LinkedHashMap.empty[CompilationError, Boolean]

    val errorsConsumer: ErrorsConsumer = {
      case err: CompilationError =>
        expectedErrors.find((matcher, alrMatched) => !alrMatched && matcher.matches(err))
          .map { (matcher, _) =>
            expectedErrors(matcher) = true
            errors.put(err, true)
          }.getOrElse {
            errors.put(err, false)
          }
      case _: String => ()
    }

    case object ExitException extends Exception

    var fatalErrorOccured = false

    def exitCalled(exitCode: ExitCode): Nothing = {
      fatalErrorOccured |= exitCode == fatalErrorExitCode
      throw ExitException
    }

    val er = ErrorReporter(errorsConsumer, exitCalled)
    val pipeline = TasksPipelines.typeChecker(er, okReporter = _ => ())
    try {
      pipeline.apply(srcFiles)
    } catch {
      case ExitException => ()
    }

    val errorsAreMissing = expectedErrors.exists(!_._2)
    val existsUnexpectedErrors = errors.exists(!_._2)
    if (errorsAreMissing || existsUnexpectedErrors || fatalErrorOccured) {
      fail(
        if fatalErrorOccured then "A fatal error occured\n\n" else "" +
          "\nExpected errors:\n" +
          expectedErrors.map(markOkOrNot).mkString("\n") +
          "\n\nActual errors:\n" +
          errors.map(markOkOrNot).mkString("\n")
      )
    }
  }

  private def markOkOrNot[T](tAndOkFlag: (T, Boolean)): String = {
    val (t, okFlag) = tAndOkFlag
    val prefix =
      if okFlag
      then (colorGreen + " ✔ " + colorReset)
      else (colorRed + " ✘ " + colorReset)
    prefix + t
  }

  private def readErrorsMatchers(lines: Seq[String], fileName: String): mutable.Set[Matcher] = {
    val matchers = mutable.Set.empty[Matcher]
    for ((line, zeroBasedLineIdx) <- lines.zipWithIndex) {
      val markerIdx = line.indexOf(matcherMarker)
      val dataIdx = markerIdx + matcherMarker.length
      if (markerIdx != -1 && dataIdx < line.length) {
        val matchersStrings = line.substring(dataIdx).split(matcherSep)
        for (matcherStr <- matchersStrings) {
          val Array(levelAndLineStr, msg) = matcherStr.split(":", 2).map(_.trim)
          val (levelStr, oneBasedColIdxOpt) = levelAndLineStr.split(colMarker, 2) match {
            case Array(lv, colStr) =>
              val colOpt = colStr.toIntOption.orElse(throw AssertionError(s"cannot parse column: $colStr"))
              (lv, colOpt)
            case Array(lv) => (lv, None)
          }
          matchers.addOne(Matcher(msg, fileName, zeroBasedLineIdx + 1, oneBasedColIdxOpt, parseLevel(levelStr)))
        }
      }
    }
    matchers
  }

  private def parseLevel(levelStr: String): Level = levelStr match {
    case "E" => Level.Error
    case "W" => Level.Warning
    case _ => throw AssertionError(s"unexpected level code: $levelStr")
  }

  private val colorRed = "\u001B[31m"
  private val colorGreen = "\u001B[32m"
  private val colorReset = "\u001B[0m"

}
