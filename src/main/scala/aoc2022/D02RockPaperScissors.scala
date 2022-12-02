package aoc2022

import utils.AocFileOps

import scala.util.{Failure, Success}

object D02RockPaperScissors extends App {
  case class Score(s: Long) extends AnyVal

  sealed trait Move
  case object Rock extends Move
  case object Paper extends Move
  case object Scissors extends Move

  val choiceMap = Map("A" -> Rock, "B" -> Paper, "C" -> Scissors, "X" -> Rock, "Y" -> Paper, "Z" -> Scissors)
  val precedence: Map[Move, Move] = Map( Rock -> Scissors, Paper -> Rock, Scissors -> Paper )
  val points: Map[Move, Int] = Map(Rock -> 1, Paper -> 2, Scissors -> 3)

  def part1(input: List[String]): Score = {
    val moves: List[(Move, Move)] = input.map { moveString =>
      val tokens = moveString.split(" ")
      choiceMap(tokens(0)) -> (choiceMap(tokens(1)))
    }
    val score = moves.foldLeft(0L) { (acc, move) =>
      val opponent = move._1
      val mine = move._2
      acc + {
        if (precedence(mine) == opponent) {
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

  val tc = List("A Y", "B X", "C Z")
  assert(Score(15) == part1(tc))

  AocFileOps
    .readStringInputFromFile("src/main/resources/aoc2022/2022D02P1Input.lst") match {
    case Success(testVector) => assert(Score(13565) == part1(testVector))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
