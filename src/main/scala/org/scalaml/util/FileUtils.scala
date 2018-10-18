package org.scalaml.util

class FileUtils {

}
import org.apache.log4j.Logger

import scala.io.Source._
import scala.util.{Failure, Success, Try}

/**
 * Basic utility singleton to read and write content from and to a file
 * @author Patrick Nicolas
 * @since 0.98 December 2, 2013
 * @see Scala for Machine Learning
 *  @version 0.99.2
 */
private[scalaml] object FileUtils {
  private val logger = Logger.getLogger("FileUtils")

  /**
   * Read the content of a file as a String
   * @param toFile Name of the file to read the content form
   * @param className Name of the class to read from
   * @return Content of the file if successful, None otherwise
   */
  def read(toFile: String, className: String): Option[String] = Try(fromFile(toFile).mkString).toOption

  /**
   * Write the content into a file. The content is defined as a string.
   * @param content content to write into a file
   * @param pathName Name of the file to read the content form
   * @param className Name of the class to read from
   * @return true is successful, false otherwise
   */
  def write(content: String, pathName: String, className: String): Boolean = {
    import java.io.PrintWriter

    import DisplayUtils._

    var printWriter: Option[PrintWriter] = None
    var status = false
    Try {
      printWriter = Some(new PrintWriter(pathName))
      printWriter.foreach(_.write(content))
      printWriter.foreach(_.flush)
      printWriter.foreach(_.close)
      status = true
    } match {
      // Catch and display exception description and return false
      case Failure(e) =>
        error(s"$className.write failed for $pathName", logger, e)
        if (printWriter.isDefined) printWriter.foreach(_.close)
        status
      case Success(s) => status
    }
  }
}
