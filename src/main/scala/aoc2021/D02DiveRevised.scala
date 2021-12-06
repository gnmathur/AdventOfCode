package aoc2021

import aoc2021.utils.FileOperations

import scala.util.{Failure, Success}

object D02DiveRevised extends App {
  // Direction type in a command
  sealed trait Direction extends Product with Serializable
  case object Forward extends Direction
  case object Down extends Direction
  case object Up extends Direction

  // Unit type in a command
  type Units = Int

  // Command ADT
  case class Command(dir: Direction, u: Units)

  /**
   * Parse the input command tokens line-by-line and translate them into a command sequence
   *
   * @param input A collection of string input commands
   * @return A parsed set to commands
   */
  def buildCourse(input: List[String]): List[Command] = {
    input.foldLeft(List[Command]()) {
      (acc, s) =>
        // Split a line into the direction command and the units displaced
        val tokens: Array[String] = s.split("\\W+")
        // Parse the text representation into our model
        (tokens(0), tokens(1)) match {
          case ("forward", v) => acc :+ Command(Forward, Integer.parseInt(v))
          case ("down", v) => acc :+ Command(Down, Integer.parseInt(v))
          case ("up", v) => acc :+ Command(Up, Integer.parseInt(v))
        }
    }
  }

  def solve(input: List[String]) = {
    val commands = buildCourse(input)
    val (hDisplacement, aim, vDisplacement) = commands.foldLeft((0, 0, 0)) {
      case (acc, cmd) => cmd.dir match {
        case Forward => (acc._1 + cmd.u, acc._2, (acc._3 + (acc._2 * cmd.u)))
        case Down => (acc._1, acc._2 + cmd.u, acc._3)
        case Up => (acc._1, acc._2 - cmd.u, acc._3)
      }
    }
    (hDisplacement * vDisplacement)
  }

  val start = System.currentTimeMillis()
  // - Test case -
  FileOperations
    .readStringInputFromFile("src/main/resources/D02P1TestInput.txt") match {
    case Success(testVector) => assert(900 == solve(testVector))
    case Failure(exception) => println("error parsing test input")
  }

  // - Problem solution -
  FileOperations
    .readStringInputFromFile("src/main/resources/D02P1Input.txt") match {
    case Success(testVector) => assert(1880593125 == solve(testVector))
    case Failure(exception) => println("error parsing test input")
  }

  println("Took " + (System.currentTimeMillis() - start) + " ms")
}
