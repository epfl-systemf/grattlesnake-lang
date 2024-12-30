package compiler

import compiler.gennames.{ClassesAndDirectoriesNames, FileExtensions}
import compiler.io.SourceFile
import compiler.pipeline.TasksPipelines
import compiler.reporting.Errors.{ErrorReporter, ExitCode}
import org.junit.Assert.*
import org.junit.{After, Test}
import org.objectweb.asm.Opcodes.V1_8

import java.io.*
import java.nio.file.{Files, Path}
import scala.reflect.ClassTag

class ReflectionBasedExecutionTests {

  private val srcDir = "src/test/res/reflection-based-execution-tests"
  private val tmpTestDir = "testtmp"
  private val javaVersionCode = V1_8

  @After
  def deleteTmpDir(): Unit = {
    deleteRecursively(new File(tmpTestDir))
  }

  @Test def subtypeEqualityCheckTest(): Unit = {
    val classes = compileAndLoadClasses("subtype_equality_check")
    val sCreatorClass = findClassWithName(classes, "SCreator")
    val sCreator = sCreatorClass.getConstructor().newInstance()
    val iCreatorClass = findClassWithName(classes, "ICreator")
    val iCreator = iCreatorClass.getConstructor(sCreatorClass).newInstance(sCreator)
    val sInstance = iCreatorClass.getMethod("createI").invoke(iCreator)
    val sClass = sInstance.getClass
    assertTrue(sClass.getName == "S")
    val iVal = sClass.getMethod("i").invoke(sInstance)
    assertEquals(42, iVal)
    for (method <- iCreatorClass.getDeclaredMethods if method.getName.startsWith("test")){
      val res = method.invoke(null)
      assertEquals(s"failed on ${method.getName}", false, res)
    }
  }

  private def failExit(exitCode: ExitCode): Nothing = {
    fail(s"exit called, exit code: $exitCode")
    throw new AssertionError("cannot happen")
  }

  private def assertBoolArraysEqual(exp: Array[Boolean], act: Array[Boolean]): Unit = {
    def convert(b: Boolean) = if b then 1 else 0

    assertArrayEquals(exp.map(convert), act.map(convert))
  }

  private def compileAndExecOneIter(
                                     srcFileName: String,
                                     testedMethodPkgName: String,
                                     testedMethodName: String,
                                     args: Any*
                                   ): Any = {
    compileAndExecSeveralIter(srcFileName, testedMethodPkgName, testedMethodName, List(args.toArray)).head
  }

  /**
   * @param srcFileName the name of the source file
   * @param testedMethodName the method to be called
   * @param argsPerIter a list of arrays, each containing the arguments to be provided to the tested function during an iteration
   * @return the values returnes by each iteration
   */
  private def compileAndExecSeveralIter(
                                         srcFileName: String,
                                         testedMethodPkgName: String,
                                         testedMethodName: String,
                                         argsPerIter: List[Array[?]]
                                       ): List[Any] = {
    val classes = compileAndLoadClasses(srcFileName)
    val testedPkgClass = findClassWithName(classes, testedMethodPkgName)
    val pkgInstance = testedPkgClass.getField(ClassesAndDirectoriesNames.packageInstanceName).get(null)
    testedPkgClass.getDeclaredMethods.find(_.getName == testedMethodName) match {
      case None => throw AssertionError(s"specified test method '$testedMethodName' does not exist")
      case Some(method) => {
        for args <- argsPerIter yield {
          method.invoke(pkgInstance, args*)
        }
      }
    }
  }

  private def findClassWithName(classes: Seq[Class[?]], name: String) = classes.find(_.getName == name).get

  private def compileAndLoadClasses(srcFileName: String): Seq[Class[?]] = {
    val tmpDir = Path.of(tmpTestDir, srcFileName)
    val errorReporter = new ErrorReporter(errorsConsumer = System.err.print, exit = failExit)
    val compiler = TasksPipelines.compiler(tmpDir, javaVersionCode, errorReporter)
    val testFile = SourceFile(s"$srcDir/$srcFileName.${FileExtensions.rattlesnake}")
    val writtenFilesPaths = compiler.apply(List(testFile))
    val classes = {
      for path <- writtenFilesPaths yield {
        val bytes = Files.readAllBytes(path)
        val className = path.getFileName.toString.takeWhile(_ != '.')
        Loader.load(className, bytes)
      }
    }
    classes
  }

  private object Loader extends ClassLoader(Thread.currentThread().getContextClassLoader) {
    def load(name: String, bytes: Array[Byte]): Class[?] = {
      super.defineClass(name, bytes, 0, bytes.length)
    }
  }

  private def getFieldValue(obj: Any, fieldName: String): Any = {
    obj.getClass.getField(fieldName).get(obj)
  }

  private def deleteRecursively(file: File): Unit = {
    val subFiles = file.listFiles()
    if (subFiles != null) {
      for file <- subFiles do {
        deleteRecursively(file)
      }
    }
    Files.delete(file.toPath)
  }

  extension (str: String) private def withHeadUppercase: String = {
    if str.isEmpty then str
    else str.head.toUpper +: str.tail
  }

}
