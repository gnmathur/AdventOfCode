package aoc2021.utils

import java.io.BufferedReader
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters._
import scala.util.{Failure, Success, Try}

object FileOperations {

  /**
   * Read lines from a file
   *
   * @param filePath file to read
   * @return A collection with all the lines read
   */
  def readStringInputFromFile(filePath: String) : Try[List[String]] = {
    Try {
      val f: BufferedReader = Files.newBufferedReader(Paths.get(filePath), Charset.forName("UTF-8"))
      f.lines().toScala(Iterator).toList
    }
  }

  /**
   * Parse a line that has a single integer on each line
   *
   * @param filePath file to read
   * @return A List of parsed integer values
   */
  def readIntInputFromFile(filePath: String): Try[List[Int]] = {
    readStringInputFromFile(filePath) match {
      case Success(value) => Try {
        value.map {
          line => Integer.parseInt(line)
        }
      }
      case Failure(exception) => Failure(exception)
    }
  }

  /**
   * Read a single CSV line and parse integer values
   *
   * @param filePath a file with comma-separated integer values
   * @return A List of parsed integer values
   */
  def readIntFromCsvFile(filePath: String): Try[List[Int]] = {
    Try {
      val f: BufferedReader = Files.newBufferedReader(Paths.get(filePath), Charset.forName("UTF-8"))
      f.lines().toScala(Iterator).toList.head.split(",").map(Integer.parseInt).toList
    }
  }
}
