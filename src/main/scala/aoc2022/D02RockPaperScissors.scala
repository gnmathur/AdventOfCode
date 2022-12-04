package aoc2022

import utils.AocFileOps

import scala.collection.immutable.BitSet
import scala.util.{Failure, Success}

object D02RockPaperScissors extends App {
  case class Score(s: Long) extends AnyVal

  sealed trait Move

  sealed trait RPS extends Move
  case object Rock extends RPS
  case object Paper extends RPS
  case object Scissors extends RPS

  sealed trait WLD extends Move
  case object Win extends WLD
  case object Lose extends WLD
  case object Draw extends WLD

  val choiceMap = Map("A" -> Rock, "B" -> Paper, "C" -> Scissors, "X" -> Rock, "Y" -> Paper, "Z" -> Scissors)
  val wldMap = Map("Y" -> Draw, "X" -> Lose, "Z" -> Win)
  val winningMoves: Map[RPS, RPS] = Map( Rock -> Scissors, Paper -> Rock, Scissors -> Paper )
  val losingMoves: Map[RPS, RPS] = Map(Scissors -> Rock, Rock -> Paper, Paper -> Scissors )
  val points: Map[RPS, Int] = Map(Rock -> 1, Paper -> 2, Scissors -> 3)

  def solvePart2(input: List[String]): Score = {
    val moves: List[(RPS, WLD)] = input.map { moveString =>
      val tokens = moveString.split(" ")
      choiceMap(tokens(0)) -> wldMap(tokens(1))
    }

    val score = moves.foldLeft(0L) { (acc, move) =>
      val opponent = move._1
      val mine = move._2

      val inferredPoints = mine match {
        case Win => points(losingMoves(opponent)) + 6
        case Lose => points(winningMoves(opponent)) + 0
        case Draw => points(opponent) + 3
      }
      acc + inferredPoints
    }
    Score(score)
  }

  def solvePart1(input: List[String]): Score = {
    val moves: List[(RPS, RPS)] = input.map { moveString =>
      val tokens = moveString.split(" ")
      choiceMap(tokens(0)) -> (choiceMap(tokens(1)))
    }
    val score = moves.foldLeft(0L) { (acc, move) =>
      val opponent = move._1
      val mine = move._2
      acc + {
        if (winningMoves(mine) == opponent) {
          points(mine) + 6
        } else if (mine == opponent) {
          points(mine) + 3
        } else {
          points(mine) + 0
        }
      }
    }
    Score(score)
  }

  // Test Case
  val tc = List("A Y", "B X", "C Z")
  assert(Score(15) == solvePart1(tc))

  // Solve Part 1 and 2
  AocFileOps
    .readStringInputFromFile("src/main/resources/aoc2022/2022D02Input.lst") match {
    case Success(testVector) =>
      assert(Score(13565) == solvePart1(testVector))
      assert(Score(12424) == solvePart2(testVector))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }

  val order = Seq("", "B X", "C Y", "A Z", "A X", "B Y", "C Z", "C X", "A Y", "B Z").zipWithIndex.toMap
  println(order)
}
