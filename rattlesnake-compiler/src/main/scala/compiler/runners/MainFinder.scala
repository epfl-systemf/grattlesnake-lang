package compiler.runners

import compiler.gennames.ClassesAndDirectoriesNames.packageInstanceName

import java.lang.reflect.{Method, Modifier}
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object MainFinder {

  def findMainClassNameAmong(writtenFilesPaths: List[Path]): Try[String] = {
    val classes = getClasses(writtenFilesPaths)
    findMainClassName(classes)
  }

  private def findMainClassName(classes: List[Class[?]]): Try[String] = {
    val mainMethods = mutable.ListBuffer.empty[Method]
    for
      clazz <- classes
      if clazz.getDeclaredFields.exists(_.getName == packageInstanceName)
      mth <- clazz.getDeclaredMethods
      if isMainMethod(mth)
    do mainMethods.addOne(mth)
    if (mainMethods.isEmpty) {
      Failure(NoSuchMethodException("no main method found"))
    } else if (mainMethods.size > 1) {
      Failure(NoSuchMethodException("more than one main methods found"))
    } else Success(mainMethods.head.getDeclaringClass.getName)
  }

  private def getClasses(writtenFilesPaths: List[Path]): List[Class[?]] = {
    val classes = {
      for path <- writtenFilesPaths yield {
        val bytes = Files.readAllBytes(path)
        val className = path.getFileName.toString.takeWhile(_ != '.')
        Loader.load(className, bytes)
      }
    }
    classes
  }

  private def isMainMethod(mth: Method): Boolean =
    Modifier.isPublic(mth.getModifiers)
      && Modifier.isStatic(mth.getModifiers)
      && mth.getReturnType == Void.TYPE
      && mth.getName == "main"
      && mth.getParameterTypes.sameElements(Array(classOf[Array[String]]))

  /** Class loader to load generated .class files when executing the `run` command */
  private object Loader extends ClassLoader(Thread.currentThread().getContextClassLoader) {
    def load(name: String, bytes: Array[Byte]): Class[?] = {
      super.defineClass(name, bytes, 0, bytes.length)
    }
  }
  
}
